{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs #-}
module Api where


import Control.Monad (guard)
import Control.Monad.Trans (liftIO)
import Happstack.Lite (ServerPart, Response, toResponse, path, ok)
import Data.Aeson (object, (.=), ToJSON, encode, toJSON)

import Helpers (
    getAllGroups, groupExists, taskExists, getTasksOfGroup2, getTask)


data ApiResponse a where ApiResponse :: (ToJSON a) => a -> ApiResponse a
instance ToJSON (ApiResponse a) where
    toJSON a = object [ "success" .= True , "data" .= a ]


listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO getAllGroups
    ok $ toResponse $ encode $ ApiResponse groups


showGroup :: ServerPart Response
showGroup = do
    path $ \(group :: String) -> do
        -- TODO: this should be converted to appropriate API response
        guard =<< liftIO (groupExists group)
        tasks <- liftIO $ getTasksOfGroup2 group
        ok $ toResponse $ encode $ ApiResponse tasks


showTask :: ServerPart Response
showTask = do
    path $ \(pid :: Integer) -> do
        -- TODO: this should be converted to appropriate API response
        guard =<< (liftIO $ taskExists pid)
        task <- liftIO $ getTask pid
        ok $ toResponse $ encode $ ApiResponse task
