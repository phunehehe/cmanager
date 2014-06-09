{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs, DeriveGeneric #-}
module Api where


import Control.Monad (guard)
import Control.Monad.Trans (liftIO)
import Happstack.Lite (ServerPart, Response, toResponse, path, ok, lookText)
import Data.Aeson (object, (.=), ToJSON, encode, toJSON)
import Data.Text.Lazy (unpack)
import System.IO.Error (tryIOError)
import GHC.Generics (Generic)
import Control.Exception (try, tryJust)

import qualified Helpers as H


data ApiResponse a where
    SuccessApiResponse :: ToJSON a => Maybe a -> ApiResponse a
    FailureApiResponse :: Error -> ApiResponse a
instance ToJSON (ApiResponse a) where
    toJSON (SuccessApiResponse maybeData) = object $ ["success" .= True] ++ d
        where
            d = case maybeData of
                Just data_ -> ["data" .= data_]
                Nothing -> []
    toJSON (FailureApiResponse error) = object ["success" .= False, "error" .= error]

data Error = Error {
    code :: String,
    message :: String
} deriving Generic
instance ToJSON Error

-- A Nothing that is an instance of ToJSON
nothing = Nothing :: Maybe ()


listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO H.getAllGroups
    ok $ toResponse $ encode $ SuccessApiResponse $ Just groups


showGroup :: ServerPart Response
showGroup = do
    path $ \(group :: String) -> do
        result <- liftIO $ tryJust myException $ H.getTasksOfGroup2 group
        ok $ toResponse $ encode $ handle result
    where
        -- TODO: centralize this mapping
        myException H.NoSuchGroup = Just ("NoSuchGroup", "The specified group does not exist.")
        myException _ = Nothing
        handle result = case result of
            Left (code, message) -> FailureApiResponse $ Error code message
            Right tasks -> SuccessApiResponse $ Just tasks


showTask :: ServerPart Response
showTask = do
    path $ \(pid :: Integer) -> do
        result <- liftIO $ tryJust myException $ H.getTask pid
        ok $ toResponse $ encode $ handle result
    where
        -- TODO: centralize this mapping
        myException H.NoSuchTask = Just ("NoSuchTask", "The specified task does not exist.")
        myException _ = Nothing
        handle result = case result of
            Left (code, message) -> FailureApiResponse $ Error code message
            Right task -> SuccessApiResponse $ Just task


addTaskToGroup :: ServerPart Response
addTaskToGroup = do
    path $ \(group :: String) -> do
        pidText <- lookText "pid"
        case reads $ unpack pidText :: [(Integer, String)] of
            [(pid, "")] -> do
                result <- liftIO $ tryJust myException $ H.addTaskToGroup pid group
                ok $ toResponse $ encode $ handle result
            _ -> ok $ toResponse $ encode $ FailureApiResponse $ Error "" "The provided PID is not valid."
    where
        -- TODO: centralize this mapping
        myException H.NoSuchGroup = Just ("NoSuchGroup", "The specified group does not exist.")
        myException H.NoSuchTask = Just ("NoSuchTask", "The specified task does not exist.")
        myException H.UnknownError = Just ("UnknownError", "Check the log for more details.")
        -- TODO: make this DRY
        handle result = case result of
            Left (code, message) -> FailureApiResponse $ Error code message
            Right _ -> SuccessApiResponse nothing
