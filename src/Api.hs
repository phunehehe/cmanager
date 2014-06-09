{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, GADTs, DeriveGeneric #-}
module Api where


import Control.Exception (tryJust)
import Control.Monad.Trans (liftIO)
import Data.Aeson (object, (.=), ToJSON, encode, toJSON)
import Data.Text.Lazy (unpack)
import GHC.Generics (Generic)
import Happstack.Lite (ServerPart, Response, toResponse, path, ok, lookText)

import qualified Helpers as H


data Error = Error {
    code :: String,
    message :: String
} deriving Generic

instance ToJSON Error


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


-- A Nothing that is an instance of ToJSON
nothing = Nothing :: Maybe ()


handle :: ToJSON a => Either Error a -> ApiResponse a
handle maybeResult = case maybeResult of
    Left error -> FailureApiResponse $ error
    Right result -> SuccessApiResponse $ Just result


toError :: H.MyException -> Error
toError H.NoSuchGroup  = Error "NoSuchGroup"  "The specified group does not exist."
toError H.NoSuchTask   = Error "NoSuchTask"   "The specified task does not exist."
toError H.UnknownError = Error "UnknownError" "Check the log for more details."


respondJSON :: ToJSON a => a -> ServerPart Response
respondJSON = ok . toResponse . encode


omniTry :: ToJSON a => [H.MyException] -> IO a -> ServerPart Response
omniTry exceptions f = do
    result <- liftIO $ tryJust myException $ f
    respondJSON $ handle result
    where
        myException exception
            | elem exception exceptions = Just $ toError exception
            | otherwise = Nothing


listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO H.getAllGroups
    respondJSON $ SuccessApiResponse $ Just groups


showGroup :: ServerPart Response
showGroup = path $ \(group :: String) -> omniTry [H.NoSuchGroup] $ H.getTasksOfGroup group


showTask :: ServerPart Response
showTask = path $ \(pid :: Integer) -> omniTry [H.NoSuchTask] $ H.getTask pid


addTaskToGroup :: ServerPart Response
addTaskToGroup = path $ \(group :: String) -> do
    pidText <- lookText "pid"
    case reads $ unpack pidText :: [(Integer, String)] of
        [(pid, "")] -> do
            omniTry [H.NoSuchGroup, H.NoSuchTask, H.UnknownError] $ H.addTaskToGroup pid group
        _ -> respondJSON $ FailureApiResponse $ toError H.NoSuchTask
