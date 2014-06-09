{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs #-}
module Api where


import Control.Monad (guard)
import Control.Monad.Trans (liftIO)
import Happstack.Lite (ServerPart, Response, toResponse, path, ok, lookText)
import Data.Aeson (object, (.=), ToJSON, encode, toJSON)
import Data.Text.Lazy (unpack)
import System.IO.Error (tryIOError)

import qualified Helpers as H


data ApiResponse a where ApiResponse :: (ToJSON a) => a -> ApiResponse a
instance ToJSON (ApiResponse a) where
    toJSON a = object [ "success" .= True , "data" .= a ]


listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO H.getAllGroups
    ok $ toResponse $ encode $ ApiResponse groups


showGroup :: ServerPart Response
showGroup = do
    path $ \(group :: String) -> do
        -- TODO: this should be converted to appropriate API response
        guard =<< liftIO (H.groupExists group)
        tasks <- liftIO $ H.getTasksOfGroup2 group
        ok $ toResponse $ encode $ ApiResponse tasks


showTask :: ServerPart Response
showTask = do
    path $ \(pid :: Integer) -> do
        -- TODO: this should be converted to appropriate API response
        guard =<< (liftIO $ H.taskExists pid)
        task <- liftIO $ H.getTask pid
        ok $ toResponse $ encode $ ApiResponse task


addTaskToGroup :: ServerPart Response
addTaskToGroup = do
    path $ \(group :: String) -> do
        pidText <- lookText "pid"
        case reads $ unpack pidText :: [(Integer, String)] of
            [(pid, "")] -> (ok . toResponse . encode) =<< (liftIO $ process pid group)
            _ -> ok $ toResponse $ encode $ ApiResponse ("The provided PID is not valid."::String)
    where
        process :: Integer -> String -> IO (ApiResponse String)
        process pid group = do
            result <- tryIOError $ H.addTaskToGroup pid group
            case result of
                Left error -> do
                    H.log $ show error
                    return $ ApiResponse $ H.userMessage error
                Right _ -> return $ ApiResponse ""
