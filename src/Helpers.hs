{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Helpers where


-- XXX: This is said to be unportable
import GHC.IO.Exception (IOException (IOError))
import Control.Exception (tryJust)
import Control.Monad (guard)
import Data.Aeson (object, (.=), ToJSON, toJSON)
import Data.List.Split (splitOn)
import GHC.Generics (Generic)
import System.Directory (doesDirectoryExist)
import System.FilePath (takeDirectory, makeRelative, (</>))
import System.FilePath.Find (find, always, fileName, (==?))
import System.IO.Error (isDoesNotExistError)
import System.IO (hPutStrLn, stderr)


-- XXX: Maybe use </> instead of inlining /
cgroup = "/sys/fs/cgroup"
proc = "/proc"


type Group = String

data Task = Task {
    pid :: Integer,
    cmdLine :: String,
    groups :: [Group]
} deriving Generic
instance ToJSON Task


userMessage :: IOError -> String
userMessage (IOError _ _ _ description _ _) = description


log :: String -> IO ()
log message = hPutStrLn stderr $ message


groupExists :: String -> IO Bool
groupExists group = doesDirectoryExist $ cgroup </> group


taskExists :: Integer -> IO Bool
taskExists pid = doesDirectoryExist $ proc </> show pid


-- XXX: Maybe checking directory is better
getAllGroups :: IO [Group]
getAllGroups = find always (fileName ==? "tasks") cgroup
    >>= return . map takeDirectory
    >>= return . map (makeRelative cgroup)


-- TODO: merge with getTasksOfGroup
getTasksOfGroup2 :: String -> IO [Task]
getTasksOfGroup2 group = do
    contents <- readFile $ cgroup </> group </> "tasks"
    -- This read assumes the file format is correct
    mapM (getTask . read) $ lines contents


getTask :: Integer -> IO Task
getTask pid = do
    cmdLine <- getCmdLine pid
    groups <- getGroupsOfTask pid
    return $ Task {pid=pid, cmdLine=cmdLine, groups=groups}


getTasksOfGroup :: String -> IO [Integer]
getTasksOfGroup group = do
    contents <- readFile $ cgroup </> group </> "tasks"
    -- This read assumes the file format is correct
    return $ map read $ lines contents


getCmdLine :: Integer -> IO String
getCmdLine pid = do
    nullTerminated <- readFile $ proc </> show pid </> "cmdline"
    return $ map replaceNull nullTerminated
    where
        replaceNull '\0' = ' '
        replaceNull c = c


getGroupsOfTask :: Integer -> IO [String]
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


addTaskToGroup :: Integer -> String -> IO ()
addTaskToGroup pid group = do
    exist <- groupExists group
    if exist
        then appendFile (cgroup </> group </> "tasks") (show pid)
        else fail "No such group"
