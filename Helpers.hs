module Helpers where


import Control.Exception (tryJust)
import Control.Monad (guard)
import System.FilePath (takeDirectory, makeRelative, (</>))
import System.FilePath.Find (find, always, fileName, (==?))
import System.IO.Error (isDoesNotExistError)


cgroup_mount_point = "/sys/fs/cgroup"


getGroups :: IO [String]
getGroups = find always (fileName ==? "tasks") cgroup_mount_point
    >>= return . map takeDirectory
    >>= return . map (makeRelative cgroup_mount_point)


getTasks :: String -> IO (Maybe [String])
getTasks group = do
    maybeContents <- tryJust (guard . isDoesNotExistError) $
        readFile (cgroup_mount_point </> group </> "tasks")
    case maybeContents of
        Left _ -> return Nothing
        Right contents -> return $ Just $ lines contents
