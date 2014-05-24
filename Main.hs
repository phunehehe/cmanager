{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Happstack.Lite (dir, nullDir, serve, ServerPart, Response, msum,
                       toResponse, path, ok, notFound)
import Happstack.Server (askRq, rqUri)
import Text.Blaze.Html5 ((!), toHtml)

import Helpers (getAllGroups, getTasksOfGroup, getCmdLine, getGroupsOfTask)
import Templates(template, groupToLi, taskToLi)


main :: IO ()
main = do
    putStrLn "Running on port 8000"
    serve Nothing myApp


myApp :: ServerPart Response
myApp = msum
  [ dir "groups" $ nullDir >> listGroups
  , dir "groups" $ showGroup
  , dir "tasks"  $ nullDir >> listTasks
  , dir "tasks"  $ showTask
  ]


listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO getAllGroups
    rq <- askRq
    ok $ toResponse $ template (rqUri rq) "Groups" $ do
        H.h1 $ "Available CGroups"
        H.ul $ forM_ groups groupToLi


showGroup :: ServerPart Response
showGroup = do
    rq <- askRq
    path $ \(group :: String) -> do
        -- If getTasksOfGroup returns Nothing chances are the group does not exist
        maybeTasks <- liftIO $ getTasksOfGroup group
        case maybeTasks of
            Just tasks ->
                ok $ toResponse $ template (rqUri rq) ("Group " ++ group) $ do
                    H.h1 $ toHtml $ "Group " ++ group
                    H.p $ "Here are tasks in this cgroup:"
                    H.ul $ forM_ tasks taskToLi
            Nothing ->
                notFound $ toResponse $ template (rqUri rq) "Oh noes!" $ do
                    H.h1 $ toHtml $ "Group not found: " ++ group


-- TODO: Maybe remove this, it's not that useful
listTasks :: ServerPart Response
listTasks = do
    rq <- askRq
    ok $ toResponse $ template (rqUri rq) "Tasks" $ do
        H.h1 $ "Available Tasks"
        H.ul $ do
            H.li $ H.a ! A.href "/task/1234" $ "1234"
            H.li $ H.a ! A.href "/task/5678" $ "5678"


showTask :: ServerPart Response
showTask = do
    rq <- askRq
    path $ \(pid :: Integer) -> do
        -- If getCmdLine returns Nothing chances are the task does not exist
        maybeCmdLine <- liftIO $ getCmdLine pid
        maybeGroups <- liftIO $ getGroupsOfTask pid
        case (maybeCmdLine, maybeGroups) of
            (Just cmdLine, Just groups) ->
                ok $ toResponse $ template (rqUri rq) ("Task " ++ show pid) $ do
                    H.h1 $ toHtml pid
                    H.p $ toHtml cmdLine
                    H.ul $ forM_ groups groupToLi
            (_, _) ->
                notFound $ toResponse $ template (rqUri rq) "Oh noes!" $ do
                    H.h1 $ toHtml $ "Task not found: " ++ show pid
