module Helpers where


import Control.Exception (tryJust)
import Control.Monad (guard)
import System.FilePath (takeDirectory, makeRelative, (</>))
import System.FilePath.Find (find, always, fileName, (==?))
import System.IO.Error (isDoesNotExistError)


-- XXX: Maybe use </> instead of inlining /
cgroup = "/sys/fs/cgroup"
proc = "/proc"


getGroups :: IO [String]
getGroups = find always (fileName ==? "tasks") cgroup
    >>= return . map takeDirectory
    >>= return . map (makeRelative cgroup)


getTasks :: String -> IO (Maybe [String])
getTasks group = do
    maybeContents <- tryJust (guard . isDoesNotExistError) $
        readFile (cgroup </> group </> "tasks")
    case maybeContents of
        Left _ -> return Nothing
        Right contents -> return $ Just $ lines contents


getCmdLine :: Integer -> IO (Maybe String)
getCmdLine pid = do
    maybeCmdLine <- tryJust (guard . isDoesNotExistError) $
        readFile (proc </> show pid </> "cmdline")
    case maybeCmdLine of
        Left _ -> return Nothing
        Right cmdLine -> return $ Just cmdLine
