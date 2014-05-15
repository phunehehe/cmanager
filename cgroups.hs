{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text, append)
import Data.Text.Lazy (unpack)
import Happstack.Lite (dir, serve, ServerPart, Response, msum, toResponse, path, ok)
import Text.Blaze.Html5 (Html, (!), toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.Monoid (mempty)


main :: IO ()
main = do
    putStrLn "Running on port 8000"
    serve Nothing myApp


myApp :: ServerPart Response
myApp = msum
  [ dir "groups" $ listGroups
  , dir "group"  $ showGroup
  , dir "tasks"  $ listTasks
  , dir "task"   $ showTask
  ]


template :: String -> Html -> Response
template title body = toResponse $
    H.docTypeHtml ! A.lang "en" $ do
        H.head $ do
            H.meta ! A.charset "utf-8"
            H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
            H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
            H.title $ toHtml title
            H.link ! A.href "css/bootstrap.min.css" ! A.rel "stylesheet"
        H.body $ do

            H.h1 "Hello, world!"
            body
            H.a ! A.href "/groups"     $ "Groups"
            H.a ! A.href "/group/test" $ "Group test"
            H.a ! A.href "/tasks"      $ "Tasks"
            H.a ! A.href "/task/1234"  $ "Task 1234"

            H.script ! A.src "https://ajax.googleapis.com/ajax/libs/jquery/1.11.0/jquery.min.js" $ mempty
            H.script ! A.src "js/bootstrap.min.js" $ mempty


listGroups :: ServerPart Response
listGroups = ok $
    template "Groups" $ do
        H.p $ "Here are the cgroups:"
        H.ul $ do
            H.li "123"
            H.li "456"


showGroup :: ServerPart Response
showGroup = path $ \(name :: String) ->
    ok $ template ("Group " ++ name) $ do
        H.h1 $ toHtml name
        H.p $ "Here are tasks in this cgroup:"
        H.ul $ do
            H.li $ "123"
            H.li $ "456"


listTasks :: ServerPart Response
listTasks = ok $
    template "Tasks" $ do
        H.p $ "Here are the tasks:"
        H.ul $ do
            H.li $ "123"
            H.li $ "456"


showTask :: ServerPart Response
showTask = path $ \(pid :: Integer) ->
    ok $ template ("Task " ++ show pid) $ do
        H.h1 $ toHtml pid
        H.p $ "full command line"
        H.p $ "group"
        H.ul $ do
            H.li $ "123"
            H.li $ "456"
