module Helpers where


import System.FilePath (takeDirectory, makeRelative)
import System.FilePath.Find (find, always, fileName, (==?))


cgroup_mount_point = "/sys/fs/cgroup"


getGroups :: IO [FilePath]
getGroups = find always (fileName ==? "tasks") cgroup_mount_point
    >>= return . map takeDirectory
    >>= return . map (makeRelative cgroup_mount_point)
