{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM_, guard)
import Control.Monad.Trans (liftIO)
import Data.Text.Lazy (unpack)
import Happstack.Lite (
    dir, nullDir, serve, ServerPart, Response, msum, toResponse, path, ok,
    notFound, method, Method (POST), lookText)
import Happstack.Server (askRq, rqUri, rqMethod, matchMethod)
import Text.Blaze.Html5 ((!), toHtml, Html)

import Helpers (
    getAllGroups, getTasksOfGroup, getCmdLine, getGroupsOfTask,
    groupExists, taskExists, readInt, addTaskToGroup)
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
        H.h1 "Available CGroups"
        H.ul $ forM_ groups groupToLi


processForm :: String -> ServerPart (Maybe Html)
processForm group = do
    rq <- askRq
    if matchMethod POST (rqMethod rq)
    then do
        pid <- return . readInt . unpack =<< lookText "pid"
        liftIO $ addTaskToGroup pid group
        return $ Just $ do
            H.div ! A.class_ "alert alert-success alert-dismissable" $ do
                H.button
                    ! A.type_ "button"
                    ! A.class_ "close"
                    ! H.dataAttribute "dismiss" "alert"
                    ! H.customAttribute "aria-hidden" "true"
                    $ "×"
                "Task "
                H.strong $ toHtml $ show pid
                " added to group "
                H.strong $ toHtml group
                "!"
    else return Nothing


showGroup :: ServerPart Response
showGroup = do
    rq <- askRq
    path $ \(group :: String) -> do
        guard =<< liftIO (groupExists group)
        maybeResult <- processForm group
        tasks <- liftIO $ getTasksOfGroup group
        ok $ toResponse $ template (rqUri rq) ("Group " ++ group) $ do
            case maybeResult of
                Just result -> result
                Nothing -> return ()
            H.h1 $ toHtml $ "Group " ++ group
            H.p "Here are tasks in this cgroup:"
            H.ul $ forM_ tasks taskToLi
            H.form ! A.method "post" $ do
                H.label ! A.for "pid" $ "Add a task:"
                H.input ! A.type_ "text" ! A.id "pid" ! A.name "pid"
                H.input ! A.type_ "submit" ! A.value "Do it!"


-- TODO: Maybe remove this, it's not that useful
listTasks :: ServerPart Response
listTasks = do
    rq <- askRq
    ok $ toResponse $ template (rqUri rq) "Tasks" $ do
        H.h1 "Available Tasks"
        H.ul $ do
            H.li $ H.a ! A.href "/task/1234" $ "1234"
            H.li $ H.a ! A.href "/task/5678" $ "5678"


showTask :: ServerPart Response
showTask = do
    rq <- askRq
    path $ \(pid :: Integer) -> do
        guard =<< (liftIO $ taskExists pid)
        cmdLine <- liftIO $ getCmdLine pid
        groups <- liftIO $ getGroupsOfTask pid
        ok $ toResponse $ template (rqUri rq) ("Task " ++ show pid) $ do
            H.h1 $ toHtml pid
            H.p $ toHtml cmdLine
            H.ul $ forM_ groups groupToLi
