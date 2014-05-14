{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Data.Maybe (fromMaybe)
import Data.Text (Text, append)
import Data.Text.Lazy (unpack)
import Happstack.Lite (dir, serve, ServerPart, Response, msum, toResponse, path, ok)
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


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
    H.html $ do
        H.head $ do
            H.title (toHtml title)
        H.body $ do
            body
            H.a ! href "/groups"     $ "Groups"
            H.a ! href "/group/test" $ "Group test"
            H.a ! href "/tasks"      $ "Tasks"
            H.a ! href "/task/1234"  $ "Task 1234"


listGroups :: ServerPart Response
listGroups = ok $
    template "Groups" $ do
        p $ "Here are the cgroups:"
        H.ul $ do
            H.li "123"
            H.li "456"


showGroup :: ServerPart Response
showGroup = path $ \(name :: String) ->
    ok $ template ("Group " ++ name) $ do
        H.h1 $ toHtml name
        p $ "Here are tasks in this cgroup:"
        H.ul $ do
            H.li $ "123"
            H.li $ "456"


listTasks :: ServerPart Response
listTasks = ok $
    template "Tasks" $ do
        p $ "Here are the tasks:"
        H.ul $ do
            H.li $ "123"
            H.li $ "456"


showTask :: ServerPart Response
showTask = path $ \(pid :: Integer) ->
    ok $ template ("Task " ++ show pid) $ do
        H.h1 $ toHtml pid
        p $ "full command line"
        p $ "group"
        H.ul $ do
            H.li $ "123"
            H.li $ "456"
