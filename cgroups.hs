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
            H.title $ toHtml title
            H.link ! A.href "/css/bootstrap.min.css" ! A.rel "stylesheet"
            H.link ! A.href "/css/dashboard.css" ! A.rel "stylesheet"
        H.body $ do
            H.div ! A.class_ "navbar navbar-inverse navbar-fixed-top" $ H.div ! A.class_ "container-fluid" $ do
                H.div ! A.class_ "navbar-header" $ do
                    H.a ! A.class_ "navbar-brand" ! A.href "#" $ "CGroups"
            H.div ! A.class_ "container-fluid" $ H.div ! A.class_ "row" $ do
                H.div ! A.class_ "col-sm-3 col-md-2 sidebar" $ do
                    H.ul ! A.class_ "nav nav-sidebar" $ do
                        H.li ! A.class_ "active" $ H.a ! A.href "/groups" $ "Groups"
                        H.li $ H.a ! A.href "/tasks" $ "Tasks"
                H.div ! A.class_ "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main" $ do
                    body
            H.script ! A.src "/js/jquery.min.js" $ mempty
            H.script ! A.src "/js/bootstrap.min.js" $ mempty


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
