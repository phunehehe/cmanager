{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Web where


import Control.Exception (tryJust)
import Control.Monad (mzero)
import Control.Monad.Trans (liftIO)
import Data.Text.Lazy (unpack)
import Happstack.Lite (ServerPart, Response, toResponse, path, ok, lookText)
import Text.Blaze.Html5 (Html)

import qualified Helpers as H
import qualified Templates as T


listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO H.getAllGroups
    ok $ toResponse $ T.listGroupTemplate groups


parseGroup :: ServerPart (H.Group)
parseGroup = path $ \(group :: H.Group) -> return group


processGroup :: H.Group -> ServerPart (Maybe Html, H.Group)
processGroup group = do
    pidText <- lookText "pid"
    case reads $ unpack pidText :: [(H.Pid, String)] of
        [(pid, "")] -> do
            html <- liftIO $ addTaskToGroup pid group
            return (Just html, group)
        _ -> return $ (Just $ T.failureTemplate pidText group $ H.toError H.NoSuchTask, group)


showGroup :: Maybe Html -> H.Group -> ServerPart Response
showGroup maybeMessage group = do
    result <- liftIO $ tryJust myException $ H.getTasksOfGroup group
    case result of
        Left _ -> mzero
        Right tasks -> ok $ toResponse $ T.showGroupTemplate maybeMessage group tasks
    where
        myException exception
            | elem exception [H.NoSuchGroup] = Just $ H.toError exception
            | otherwise = Nothing


parsePid :: ServerPart H.Pid
parsePid = path $ \(pid :: H.Pid) -> return pid


processTask :: H.Pid -> ServerPart (Maybe Html, H.Pid)
processTask pid = do
    groupText <- lookText "group"
    html <- liftIO $ addTaskToGroup pid $ unpack groupText
    return (Just html, pid)


showTask :: Maybe Html -> H.Pid -> ServerPart Response
showTask maybeMessage pid = do
    result <- liftIO $ tryJust myException $ H.getTask pid
    case result of
        Left _ -> mzero
        Right task -> do
            allGroups <- liftIO $ H.getAllGroups
            ok $ toResponse $ T.showTaskTemplate maybeMessage task allGroups $ H.groups task
    where
        myException exception
            | elem exception [H.NoSuchGroup] = Just $ H.toError exception
            | otherwise = Nothing


addTaskToGroup :: H.Pid -> H.Group -> IO Html
addTaskToGroup pid group = do
    result <- tryJust myException $ H.addTaskToGroup pid group
    case result of
        Left error -> return $ T.failureTemplate pid group error
        Right _ -> return $ T.successTemplate pid group
    where
        myException exception
            | elem exception [H.NoSuchTask, H.NoSuchGroup, H.UnknownError] = Just $ H.toError exception
            | otherwise = Nothing
