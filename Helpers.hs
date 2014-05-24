module Helpers where


import Control.Exception (tryJust)
import Control.Monad (guard)
import Data.List.Split (splitOn)
import System.FilePath (takeDirectory, makeRelative, (</>))
import System.FilePath.Find (find, always, fileName, (==?))
import System.IO.Error (isDoesNotExistError)


-- XXX: Maybe use </> instead of inlining /
cgroup = "/sys/fs/cgroup"
proc = "/proc"


getAllGroups :: IO [String]
getAllGroups = find always (fileName ==? "tasks") cgroup
    >>= return . map takeDirectory
    >>= return . map (makeRelative cgroup)


getTasksOfGroup :: String -> IO (Maybe [Integer])
getTasksOfGroup group = do
    maybeContents <- tryJust (guard . isDoesNotExistError) $
        readFile $ cgroup </> group </> "tasks"
    case maybeContents of
        Left _ -> return Nothing
        Right contents -> return $ Just $ map read $ lines contents


getCmdLine :: Integer -> IO (Maybe String)
getCmdLine pid = do
    maybeCmdLine <- tryJust (guard . isDoesNotExistError) $
        readFile $ proc </> show pid </> "cmdline"
    case maybeCmdLine of
        Left _ -> return Nothing
        Right cmdLine -> return $ Just cmdLine


getGroupsOfTask :: Integer -> IO (Maybe [String])
getGroupsOfTask pid = do
    maybeContents <- tryJust (guard . isDoesNotExistError) $
        readFile $ proc </> show pid </> "cgroup"
    case maybeContents of
        Left _ -> return Nothing
        Right contents -> return $ Just $ map convertOne $ lines contents
    where
        -- One line is like this
        -- 4:memory:/awesome_group
        convertOne line = parts!!1 ++ parts!!2
            where parts = splitOn ":" line
