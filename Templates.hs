{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Templates where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.List (isPrefixOf)
import Data.Monoid (mempty)
import Network.HTTP.Base (urlEncode)
import System.FilePath ((</>))
import Text.Blaze.Html5 (Html, (!), toHtml, toValue)

import Helpers (getAllGroups, getTasksOfGroup, getCmdLine, getGroupsOfTask)


template :: String -> String -> Html -> Html
template url title body = H.docTypeHtml ! A.lang "en" $ do
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


groupToLi :: String -> Html
groupToLi group = H.li $ H.a ! A.href (toValue $ "/groups" </> urlEncode group)
                             $ toHtml group


-- TODO: Actually this should be Integer, not String
taskToLi :: String -> Html
taskToLi task = H.li $ H.a ! A.href (toValue $ "/tasks" </> task)
                           $ toHtml task
