{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs #-}
module Api where


import Control.Monad.Trans (liftIO)
import Data.Aeson (object, (.=), ToJSON, encode, toJSON)
import Data.Text.Lazy (unpack)
import Happstack.Lite (ServerPart, Response, toResponse, ok, lookText)

import qualified Helpers as H


-- Data structure to generate JSON using Aeson
data ApiResponse a where
    SuccessApiResponse :: ToJSON a => Maybe a -> ApiResponse a
    FailureApiResponse :: H.Error -> ApiResponse a

instance ToJSON (ApiResponse a) where
    toJSON (SuccessApiResponse maybeData) = object $
        ("success" .= True) :
        case maybeData of
            Just data_ -> ["data" .= data_]
            Nothing -> []
    toJSON (FailureApiResponse error) = object
        ["success" .= False, "error" .= error]


-- Function aliases to save some space
apiResponse :: ApiResponse a -> ServerPart Response
apiResponse = ok . toResponse . encode


-- API endpoint to list all groups
listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO H.getAllGroups
    apiResponse $ SuccessApiResponse $ Just groups


-- API endpoint to list PIDs of tasks in a group
-- Parameter group is supplied by parseGroup in Main
showGroup :: H.Group -> ServerPart Response
showGroup group = do
    maybePids <- liftIO $ H.getTasksOfGroup group
    case maybePids of
        Left error -> apiResponse $ FailureApiResponse error
        Right pids -> apiResponse $ SuccessApiResponse $ Just pids


-- API endpoint to show details of a task
-- Parameter pid is supplied by parsePid in Main
showTask :: H.Pid -> ServerPart Response
showTask pid = do
    maybeTask <- liftIO $ H.getTask pid
    case maybeTask of
        Left error -> apiResponse $ FailureApiResponse error
        Right task -> apiResponse $ SuccessApiResponse $ Just task


-- API endpoint to add a task to a group
-- Parameter group is supplied by parseGroup in Main
-- pid is a POST parameter
addTaskToGroup :: H.Group -> ServerPart Response
addTaskToGroup group = do
    pidText <- lookText "pid"
    case reads $ unpack pidText :: [(H.Pid, String)] of
        [(pid, "")] -> do
            result <- liftIO $ H.addTaskToGroup pid group
            case result of
                Left error -> apiResponse $ FailureApiResponse error
                Right _ -> apiResponse $ SuccessApiResponse (Nothing :: Maybe String)
        _ -> apiResponse $ FailureApiResponse $ H.toError H.NoSuchTask
