{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs, DeriveGeneric #-}
module Api where


import Control.Exception (tryJust)
import Control.Monad.Trans (liftIO)
import Data.Aeson (object, (.=), ToJSON, encode, toJSON)
import Data.Text.Lazy (unpack)
import GHC.Generics (Generic)
import Happstack.Lite (ServerPart, Response, toResponse, path, ok, lookText)

import qualified Helpers as H


data ApiResponse a where
    SuccessApiResponse :: ToJSON a => Maybe a -> ApiResponse a
    FailureApiResponse :: H.Error -> ApiResponse a

instance ToJSON (ApiResponse a) where
    toJSON (SuccessApiResponse maybeData) = object $ ["success" .= True] ++ d
        where
            d = case maybeData of
                Just data_ -> ["data" .= data_]
                Nothing -> []
    toJSON (FailureApiResponse error) = object ["success" .= False, "error" .= error]


-- A Nothing that is an instance of ToJSON
nothing = Nothing :: Maybe ()


handle :: ToJSON a => Either H.Error a -> ApiResponse a
handle maybeResult = case maybeResult of
    Left error -> FailureApiResponse $ error
    Right result -> SuccessApiResponse $ Just result


respondJSON :: ToJSON a => a -> ServerPart Response
respondJSON = ok . toResponse . encode


omniTry :: ToJSON a => [H.MyException] -> IO a -> ServerPart Response
omniTry exceptions f = do
    result <- liftIO $ tryJust myException $ f
    respondJSON $ handle result
    where
        myException exception
            | elem exception exceptions = Just $ H.toError exception
            | otherwise = Nothing


listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO H.getAllGroups
    respondJSON $ SuccessApiResponse $ Just groups


showGroup :: ServerPart Response
showGroup = path $ \(group :: String) -> omniTry [H.NoSuchGroup] $ H.getTasksOfGroup group


showTask :: ServerPart Response
showTask = path $ \(pid :: H.Pid) -> omniTry [H.NoSuchTask] $ H.getTask pid


addTaskToGroup :: ServerPart Response
addTaskToGroup = path $ \(group :: String) -> do
    pidText <- lookText "pid"
    case reads $ unpack pidText :: [(H.Pid, String)] of
        [(pid, "")] -> do
            omniTry [H.NoSuchGroup, H.NoSuchTask, H.UnknownError] $ H.addTaskToGroup pid group
        _ -> respondJSON $ FailureApiResponse $ H.toError H.NoSuchTask
