{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Web where


import Control.Exception (tryJust)
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


shimGroup :: ServerPart (Maybe Html)
shimGroup = do
    path $ \(group :: H.Group) -> do
        pidText <- lookText "pid"
        case reads $ unpack pidText :: [(H.Pid, String)] of
            [(pid, "")] -> return . Just =<< addTaskToGroup pid group
            _ -> return $ Just $ T.failureTemplate pidText group $ H.toError H.NoSuchTask


shimTask :: ServerPart (Maybe Html)
shimTask = do
    path $ \(pid :: H.Pid) -> do
        groupText <- lookText "group"
        return . Just =<< (addTaskToGroup pid $ unpack groupText)


addTaskToGroup :: H.Pid -> H.Group -> ServerPart Html
addTaskToGroup pid group = do
    result <- liftIO $ tryJust myException $ H.addTaskToGroup pid group
    case result of
        Left error -> return $ T.failureTemplate pid group error
        Right _ -> return $ T.successTemplate pid group
    where
        myException exception
            | elem exception [H.NoSuchTask, H.NoSuchGroup, H.UnknownError] = Just $ H.toError exception
            | otherwise = Nothing


showGroup :: Maybe Html -> ServerPart Response
showGroup maybeMessage = do
    -- XXX: parsing twice
    path $ \(group :: String) -> do
        -- TODO: handle NoSuchGroup
        tasks <- liftIO $ H.getTasksOfGroup group
        ok $ toResponse $ T.showGroupTemplate maybeMessage group tasks


showTask :: Maybe Html -> ServerPart Response
showTask maybeMessage = do
    -- XXX: parsing twice
    path $ \(pid :: H.Pid) -> do
        -- TODO: handle NoSuchTask
        task <- liftIO $ H.getTask pid
        allGroups <- liftIO $ H.getAllGroups
        ok $ toResponse $ T.showTaskTemplate maybeMessage task allGroups $ H.groups task
