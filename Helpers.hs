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


groupExists :: String -> IO Bool
groupExists group = doesDirectoryExist $ cgroup </> group


taskExists :: Integer -> IO Bool
taskExists pid = doesDirectoryExist $ proc </> show pid


-- XXX: Maybe checking directory is better
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
