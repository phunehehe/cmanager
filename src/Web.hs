{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Web where


import Control.Monad (mzero)
import Control.Monad.Trans (liftIO)
import Data.Text.Lazy (unpack)
import Happstack.Lite (ServerPart, Response, toResponse, ok, lookText)
import Text.Blaze.Html5 (Html)

import qualified Helpers as H
import qualified Templates as T


-- Add a task to a group, returning a HTML message
addTaskToGroup :: H.Pid -> H.Group -> IO Html
addTaskToGroup pid group = do
    result <- H.addTaskToGroup pid group
    case result of
        Left error -> return $ T.failureTemplate pid group error
        Right _ -> return $ T.successTemplate pid group


-- Provided a group in the URL, parse a PID from POST parameters and add it to
-- the group
-- The resulting Html needs to be wrapped in Just because it will be passed to
-- showGroup, which expects a Maybe
processGroup :: H.Group -> ServerPart (Maybe Html, H.Group)
processGroup group = do
    pidText <- lookText "pid"
    case reads $ unpack pidText :: [(H.Pid, String)] of
        [(pid, "")] -> do
            html <- liftIO $ addTaskToGroup pid group
            return (Just html, group)
        _ -> return (Just $ T.failureTemplate pidText group $ H.toError H.NoSuchTask, group)


-- Provided a PID in the URL, parse a group from POST parameters and add the
-- PID to the group
-- The resulting Html needs to be wrapped in Just because it will be passed to
-- showTask, which expects a Maybe
processTask :: H.Pid -> ServerPart (Maybe Html, H.Pid)
processTask pid = do
    groupText <- lookText "group"
    html <- liftIO $ addTaskToGroup pid $ unpack groupText
    return (Just html, pid)


-- Web endpoint to list all groups
listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO H.getAllGroups
    ok $ toResponse $ T.listGroupTemplate groups


-- Web endpoint to list PIDs of tasks in a group
showGroup :: Maybe Html -> H.Group -> ServerPart Response
showGroup maybeMessage group = do
    maybePids <- liftIO $ H.getTasksOfGroup group
    case maybePids of
        Left _ -> mzero
        Right pids -> ok $ toResponse $ T.showGroupTemplate maybeMessage group pids


-- Web endpoint to show details of a task
showTask :: Maybe Html -> H.Pid -> ServerPart Response
showTask maybeMessage pid = do
    maybeTask <- liftIO $  H.getTask pid
    case maybeTask of
        Left _ -> mzero
        Right task -> do
            allGroups <- liftIO H.getAllGroups
            ok $ toResponse $ T.showTaskTemplate maybeMessage task allGroups $ H.groups task
