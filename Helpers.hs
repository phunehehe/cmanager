module Helpers where


import Control.Exception (tryJust)
import Control.Monad (guard)
import Data.List.Split (splitOn)
import System.FilePath (takeDirectory, makeRelative, (</>))
import System.FilePath.Find (find, always, fileName, (==?))
import System.IO.Error (isDoesNotExistError)
import System.Directory (doesDirectoryExist)


-- XXX: Maybe use </> instead of inlining /
cgroup = "/sys/fs/cgroup"
proc = "/proc"


readInt :: String -> Integer
readInt = read


groupExists :: String -> IO Bool
groupExists group = doesDirectoryExist $ cgroup </> group


taskExists :: Integer -> IO Bool
taskExists pid = doesDirectoryExist $ proc </> show pid


getAllGroups :: IO [String]
getAllGroups = find always (fileName ==? "tasks") cgroup
    >>= return . map takeDirectory
    >>= return . map (makeRelative cgroup)


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
        convertOne line = parts!!1 ++ parts!!2
            where parts = splitOn ":" line


addTaskToGroup :: Integer -> String -> IO ()
addTaskToGroup pid group = do
    exist <- groupExists group
    if exist
        then appendFile (cgroup </> group </> "tasks") (show pid)
        else fail "No such group"
