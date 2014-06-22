{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Helpers where


import Control.Exception (tryJust)
import Control.Monad (guard)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Either (runEitherT, hoistEither)
import Data.Aeson (ToJSON)
import Data.List (isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, canonicalizePath)
import System.FilePath (takeDirectory, makeRelative, (</>))
import System.FilePath.Find (find, always, fileName, (==?))
import System.IO (hPrint, stderr)
import System.IO.Error (tryIOError, isDoesNotExistError)


-- File system paths
cgroup = "/sys/fs/cgroup"
proc = "/proc"


-- Type aliases to make it more obvious what we are dealing with
type Group = String
type Pid = Integer


-- Data structure to generate JSON using Aeson
data Task = Task {
    pid :: Pid,
    cmdLine :: String,
    groups :: [Group]
} deriving (Show, Generic)

instance ToJSON Task


-- Data structure to generate JSON using Aeson
data Error = Error {
    code :: String,
    message :: String
} deriving Generic

instance ToJSON Error


-- Application specific exceptions
data MyException = NoSuchGroup | NoSuchTask | UnknownError
    deriving (Show, Eq)


-- Convert an exception to error code and message
toError :: MyException -> Error
toError NoSuchGroup  = Error "NoSuchGroup"  "The specified group does not exist."
toError NoSuchTask   = Error "NoSuchTask"   "The specified task does not exist."
toError UnknownError = Error "UnknownError" "Check the log for more details."


-- Check whether a group exists
groupExists :: String -> IO Bool
groupExists group = do
    result <- tryJust notExist $ canonicalizePath $ cgroup </> group
    case result of
        Left _ -> return False
        -- Make sure that we are looking at something under /sys/fs/cgroup
        Right fullPath -> return $ isPrefixOf cgroup fullPath
    where
        notExist = guard . isDoesNotExistError


-- Check whether a task exist
taskExists :: Pid -> IO Bool
taskExists pid = doesDirectoryExist $ proc </> show pid


-- Look for all available groups
getAllGroups :: IO [Group]
getAllGroups = do
    taskFiles <- find always (fileName ==? "tasks") cgroup
    return $ map (makeRelative cgroup . takeDirectory) taskFiles


-- Look for all PIDs of tasks in a group
getTasksOfGroup :: String -> IO (Either Error [Pid])
getTasksOfGroup group = do
    -- FIXME: This is not atomic
    exist <- groupExists group
    if exist
        then do
            contents <- readFile $ cgroup </> group </> "tasks"
            return $ Right $ map read $ lines contents
        else return $ Left $ toError NoSuchGroup


-- Look for details of a task
getTask :: Pid -> IO (Either Error Task)
getTask pid = do
    -- FIXME: This is not atomic
    exist <- taskExists pid
    if exist
        then do
            cmdLine <- getCmdLine pid
            groups <- getGroupsOfTask pid
            return $ Right Task {pid=pid, cmdLine=cmdLine, groups=groups}
        else return $ Left $ toError NoSuchTask


-- Look for the command line of a task
-- Return an empty string when this information is not available
getCmdLine :: Pid -> IO String
getCmdLine pid = do
    nullTerminated <- readFile $ proc </> show pid </> "cmdline"
    return $ map replaceNull nullTerminated
    where
        replaceNull '\0' = ' '
        replaceNull c = c


-- Look for the groups that a task belongs to, treating the subsystems
-- controlling a group as a part of the group's name
getGroupsOfTask :: Pid -> IO [String]
getGroupsOfTask pid = do
    cgroupsRaw <- readFile $ proc </> show pid </> "cgroup"
    mountsRaw <- readFile $ proc </> "mounts"
    let mounts = filter isCgroup . map words $ lines mountsRaw
    return $ map (lineToGroup mounts) $ lines cgroupsRaw
    where

        -- Check whether a mount entry is for a CGroup
        isCgroup = (==) "cgroup" . mountType
        -- Check whether a mount entry contains given hierarchies
        -- It was observed that CGroup information is always at the end of
        -- mount options, even though there is no supporting documentation.
        -- TODO: What if group names overlap?
        containsHiers hiers = isSuffixOf hiers . mountOptions

        -- Each entry in /proc/mounts has the form
        -- cgroup /sys/fs/cgroup/cpu,cpuacct cgroup rw,nosuid,nodev,noexec,relatime,cpuacct,cpu 0 0
        mountTarget mount = mount !! 1
        mountType mount = mount !! 2
        mountOptions mount = mount !! 3

        -- Look for the group name of a line in /proc/PID/cgroup
        -- Each cgroup line has the form:
        -- 4:memory:/awesome_group
        lineToGroup mounts line
            | path == "/" = myHier
            | otherwise = myHier ++ path
            where
                _:hiers:path:[] = splitOn ":" line
                myMount = head $ filter (containsHiers hiers) mounts
                myHier = makeRelative cgroup $ mountTarget myMount


-- Try to add a task to a group
addTaskToGroup :: Pid -> String -> IO (Either Error ())
addTaskToGroup pid group = runEitherT $ do
    -- FIXME: This is not atomic
    exist <- liftIO $ taskExists pid
    hoistEither $ if not exist
        then Left $ toError NoSuchTask
        else Right ()
    exist <- liftIO $ groupExists group
    hoistEither $ if not exist
        then Left $ toError NoSuchGroup
        else Right ()
    result <- lift $ tryIOError $ appendFile (cgroup </> group </> "tasks") (show pid)
    case result of
        Left exception -> do
            lift $ hPrint stderr exception
            hoistEither $ Left $ toError UnknownError
        Right _ -> hoistEither $ Right ()
