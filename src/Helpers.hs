{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
module Helpers where


import qualified System.FilePath.Find as F

import Control.Exception (tryJust)
import Control.Monad (guard)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.Either (runEitherT, hoistEither)
import Data.Aeson (ToJSON)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, canonicalizePath)
import System.FilePath (makeRelative, (</>))
import System.FilePath.Find ((>?), (&&?), (==?))
import System.IO (hPutStrLn, stderr)
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
getAllGroups =
    -- The depth limit is needed so that the /sys/fs/cgroup root directory
    -- won't be included
    F.find F.always (F.fileType ==? F.Directory &&? F.depth >? 1) cgroup
    >>= return . map (makeRelative cgroup)


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
-- XXX: Sometimes subsystems are listed in different orders (e.g. cpu,cpuacct
-- in /sys/fs/cgroup and cpuacct,cpu in /proc/PID/cgroup). This will result in
-- NoSuchGroup errors.
getGroupsOfTask :: Pid -> IO [String]
getGroupsOfTask pid = do
    contents <- readFile $ proc </> show pid </> "cgroup"
    return $ map convertOne $ lines contents
    where
        -- Each line has the form:
        -- 4:memory:/awesome_group
        -- However sometimes the line is like this for no documented reason:
        -- 2:name=systemd:/user.slice/user-1000.slice/session-4.scope
        convertOne line
            | path == "/" = hier
            | otherwise = hier ++ path
            where
                realHier tempHier = last $ splitOn "=" tempHier
                _:tempHier:path:[] = splitOn ":" line
                hier = realHier tempHier


-- Try to add a task to a group
addTaskToGroup :: Pid -> String -> IO (Either Error ())
addTaskToGroup pid group = runEitherT $ do
    -- FIXME: This is not atomic
    exist <- liftIO $ taskExists pid
    if not exist
        then hoistEither $ Left $ toError NoSuchTask
        else hoistEither $ Right ()
    exist <- liftIO $ groupExists group
    if not exist
        then hoistEither $ Left $ toError NoSuchGroup
        else hoistEither $ Right ()
    result <- lift $ tryIOError $ appendFile (cgroup </> group </> "tasks") (show pid)
    case result of
        Left exception -> do
            lift $ hPutStrLn stderr $ show exception
            hoistEither $ Left $ toError UnknownError
        Right _ -> hoistEither $ Right ()
