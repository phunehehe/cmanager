{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Exception (tryJust)
import Control.Monad (forM_, guard)
import Control.Monad.Trans (liftIO)
import Data.List ((\\))
import Data.Text.Lazy (unpack)
import Happstack.Lite (
    dir, nullDir, serve, ServerPart, Response, msum, toResponse, path, ok,
    Method (POST), lookText)
import Happstack.Server (askRq, rqUri, rqMethod, matchMethod)
import System.IO.Error (isDoesNotExistError)
import Text.Blaze.Html5 ((!), toHtml, toValue, Html)

import Helpers (
    getAllGroups, getTasksOfGroup, getCmdLine, getGroupsOfTask,
    groupExists, taskExists, readInt, addTaskToGroup)
import Templates(template, groupToLi, taskToLi, alert)


main :: IO ()
main = do
    putStrLn "Running on port 8000"
    serve Nothing myApp


myApp :: ServerPart Response
myApp = msum
  [ dir "groups" $ nullDir >> listGroups
  , dir "groups" $ showGroup
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
    if matchMethod POST $ rqMethod rq
    then do
        pidText <- lookText "pid"
        -- TODO: validate pid
        let pid = readInt $ unpack pidText
        result <- liftIO $ tryJust notExist $ addTaskToGroup pid group
        case result of
            Left message -> return $ Just $ alert "danger" $ do
                "Something was wrong when adding task "
                pidToHtml pid
                " to group "
                groupToHtml group
                ":"
                H.br
                toHtml message
            Right _ -> return $ Just $ alert "success" $ do
                "Task "
                pidToHtml pid
                " has been added to group "
                groupToHtml group
                "."
    else return Nothing
    where
        groupToHtml :: String -> Html
        groupToHtml group = H.strong $ toHtml group

        pidToHtml :: Integer -> Html
        pidToHtml pid = H.strong $ toHtml pid

        notExist :: IOError -> Maybe String
        notExist exception
            | isDoesNotExistError exception = Just $ show exception
            | otherwise = Nothing


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


showTask :: ServerPart Response
showTask = do
    rq <- askRq
    path $ \(pid :: Integer) -> do
        guard =<< (liftIO $ taskExists pid)
        cmdLine <- liftIO $ getCmdLine pid
        allGroups <- liftIO $ getAllGroups
        belongingGroups <- liftIO $ getGroupsOfTask pid
        ok $ toResponse $ template (rqUri rq) ("Task " ++ show pid) $ do
            H.h1 $ toHtml pid
            H.p $ toHtml cmdLine
            H.ul $ forM_ belongingGroups groupToLi
            showForm allGroups belongingGroups
    where
        showForm :: [String] -> [String] -> Html
        showForm allGroups belongingGroups = do
            H.form ! A.method "post" $ do
                H.label ! A.for "group" $ "Add to group:"
                H.select ! A.id "group" ! A.name "group" $ do
                    forM_ (allGroups \\ belongingGroups) groupToOption
                H.input ! A.type_ "submit" ! A.value "Do it!"

        groupToOption :: String -> Html
        groupToOption group = H.option ! A.value (toValue group) $ toHtml group
