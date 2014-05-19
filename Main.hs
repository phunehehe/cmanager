{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad (forM_)
import Control.Monad.Trans (liftIO)
import Data.List (isPrefixOf)
import Data.Monoid (mempty)
import Happstack.Lite (dir, serve, ServerPart, Response, msum, toResponse, path, ok)
import Happstack.Server (askRq, rqUri)
import Network.HTTP.Base (urlEncode)
import Text.Blaze.Html5 (Html, (!), toHtml, toValue)

import Helpers (getGroups)


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


template :: String -> String -> Html -> Response
template url title body = toResponse $
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
                        maybeActive "/groups" "Groups"
                        maybeActive "/tasks" "Tasks"
                H.div ! A.class_ "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main" $ do
                    body
            H.script ! A.src "/js/jquery.min.js" $ mempty
            H.script ! A.src "/js/bootstrap.min.js" $ mempty
    where
        maybeActive :: String -> String -> Html
        maybeActive targetUrl text
            | isPrefixOf targetUrl url = H.li ! A.class_ "active" $ anchor
            | otherwise = H.li $ anchor
            where anchor = H.a ! A.href (toValue targetUrl) $ toHtml text


listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO getGroups
    rq <- askRq
    ok $ template (rqUri rq) "Groups" $ do
        H.h1 $ "Available CGroups"
        H.ul $ forM_ groups groupToLi
    where
        groupToLi :: String -> Html
        groupToLi group = H.li $ H.a ! A.href (toValue $ "group/" ++ urlEncode group)
                                     $ toHtml group



showGroup :: ServerPart Response
showGroup = do
    rq <- askRq
    path $ \(name :: String) ->
        ok $ template (rqUri rq) ("Group " ++ name) $ do
            H.h1 $ toHtml name
            H.p $ "Here are tasks in this cgroup:"
            H.ul $ do
                H.li $ H.a ! A.href "/task/1234" $ "1234"
                H.li $ H.a ! A.href "/task/5678" $ "5678"


listTasks :: ServerPart Response
listTasks = do
    rq <- askRq
    ok $ template (rqUri rq) "Tasks" $ do
        H.h1 $ "Available Tasks"
        H.ul $ do
            H.li $ H.a ! A.href "/task/1234" $ "1234"
            H.li $ H.a ! A.href "/task/5678" $ "5678"


showTask :: ServerPart Response
showTask = do
    rq <- askRq
    path $ \(pid :: Integer) ->
        ok $ template (rqUri rq) ("Task " ++ show pid) $ do
            H.h1 $ toHtml pid
            H.p $ "TODO: show full command line here"
            H.a ! A.href "/group/amazing_group" $ "amazing_group"
