{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveDataTypeable #-}
module Helpers where


import Control.Exception (throw, Exception)
import Control.Exception (tryJust)
import Data.Aeson (object, (.=), ToJSON, toJSON)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist, canonicalizePath)
import System.FilePath (takeDirectory, makeRelative, (</>))
import System.FilePath.Find (find, always, fileName, (==?))
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError, isDoesNotExistError)


-- XXX: Maybe use </> instead of inlining /
cgroup = "/sys/fs/cgroup"
proc = "/proc"


type Group = String
type Pid = Integer


data Task = Task {
    pid :: Pid,
    cmdLine :: String,
    groups :: [Group]
} deriving (Show, Generic)

instance ToJSON Task


data MyException = NoSuchGroup | NoSuchTask | UnknownError
    deriving (Show, Typeable, Eq)

instance Exception MyException


data Error = Error {
    code :: String,
    message :: String
} deriving Generic

instance ToJSON Error


toError :: MyException -> Error
toError NoSuchGroup  = Error "NoSuchGroup"  "The specified group does not exist."
toError NoSuchTask   = Error "NoSuchTask"   "The specified task does not exist."
toError UnknownError = Error "UnknownError" "Check the log for more details."


log :: String -> IO ()
log message = hPutStrLn stderr $ message


groupExists :: String -> IO Bool
groupExists group = do
    result <- tryJust notExist $ canonicalizePath $ cgroup </> group
    case result of
        Left _ -> return False
        Right fullPath -> return $ isPrefixOf cgroup fullPath
    where
        notExist exception
            | isDoesNotExistError exception = Just exception
            | otherwise = Nothing


taskExists :: Pid -> IO Bool
taskExists pid = doesDirectoryExist $ proc </> show pid


-- XXX: Maybe checking directory is better
getAllGroups :: IO [Group]
getAllGroups = find always (fileName ==? "tasks") cgroup
    >>= return . map (makeRelative cgroup . takeDirectory)


getTasksOfGroup :: String -> IO [Task]
getTasksOfGroup group = do
    -- FIXME: This is not atomic
    exist <- groupExists group
    if exist
        then do
            contents <- readFile $ cgroup </> group </> "tasks"
            mapM (getTask . read) $ lines contents
        else throw NoSuchGroup


getTask :: Pid -> IO Task
getTask pid = do
    -- FIXME: This is not atomic
    exist <- taskExists pid
    if exist
        then do
            cmdLine <- getCmdLine pid
            groups <- getGroupsOfTask pid
            return $ Task {pid=pid, cmdLine=cmdLine, groups=groups}
        else throw NoSuchTask


getCmdLine :: Pid -> IO String
getCmdLine pid = do
    nullTerminated <- readFile $ proc </> show pid </> "cmdline"
    return $ map replaceNull nullTerminated
    where
        replaceNull '\0' = ' '
        replaceNull c = c


-- TODO: this does not handle cpu,cpuacct vs cpuacct,cpu
getGroupsOfTask :: Pid -> IO [String]
getGroupsOfTask pid = do
    contents <- readFile $ proc </> show pid </> "cgroup"
    return $ map convertOne $ lines contents
    where
        -- One line is like this
        -- 4:memory:/awesome_group
        convertOne line
            | path == "/" = hier
            | otherwise = hier ++ path
            where
                _:tempHier:path:[] = splitOn ":" line
                hier = realHier tempHier
        -- Sometimes the line is like this for no documented reason
        -- 2:name=systemd:/user.slice/user-1000.slice/session-4.scope
        realHier tempHier = last $ splitOn "=" tempHier


addTaskToGroup :: Pid -> String -> IO (Maybe ())
addTaskToGroup pid group = do
    -- FIXME: This is not atomic
    exist <- taskExists pid
    if not exist
        then throw NoSuchTask
        else return Nothing
    exist <- groupExists group
    if not exist
        then throw NoSuchGroup
        else return Nothing
    result <- tryIOError $ appendFile (cgroup </> group </> "tasks") (show pid)
    case result of
        Left exception -> do
            Helpers.log $ show exception
            throw UnknownError
        Right _ -> return Nothing
