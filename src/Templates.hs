{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Templates where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Data.List ((\\))
import Control.Monad (forM_)
import Data.List (isPrefixOf)
import Data.Monoid (mempty)
import Network.HTTP.Base (urlEncode)
import System.FilePath ((</>))
import Text.Blaze.Html5 (Html, (!), toHtml, toValue)
import Text.Printf (printf)

import qualified Helpers as E


-- Because pid may not be a valid PID
failureTemplate :: Show a => a -> E.Group -> E.Error -> Html
failureTemplate pid group error = alert "danger" $ do
    "Something was wrong when adding task "
    pidToHtml pid
    " to group "
    groupToHtml group
    ":"
    H.ul $ H.li $ toHtml $ E.message error


successTemplate :: E.Pid -> E.Group -> Html
successTemplate pid group = alert "success" $ do
    "Task "
    pidToHtml pid
    " has been added to group "
    groupToHtml group
    "."


pidToHtml :: Show a => a -> Html
pidToHtml pid = H.strong $ toHtml $ show pid


groupToHtml :: String -> Html
groupToHtml group = H.strong $ toHtml group


listGroupTemplate :: [E.Group] -> Html
listGroupTemplate groups = template "All Groups" $ do
    H.h1 "All Groups"
    H.ul $ forM_ groups groupToLi


showGroupTemplate :: (Maybe Html) -> E.Group -> [E.Task] -> Html
showGroupTemplate maybeMessage group tasks = template title $ do
    case maybeMessage of
        Just message -> message
        Nothing -> mempty
    H.h1 $ toHtml title
    H.div ! A.class_ "row" $ do
        H.div ! A.class_ "col-md-6" $ do
            H.p "Here are the tasks in this group:"
            H.ul $ forM_ tasks taskToLi
        H.div ! A.class_ "col-md-6" $ do
            H.form ! A.method "POST" ! A.class_ "form-inline" $ H.fieldset $ do
                H.legend "Add a task to this group"
                H.input ! A.name "pid" ! A.type_ "text"
                    ! A.class_ "form-control"
                    ! A.placeholder "PID of a running task"
                -- FIXME: A silly space is needed otherwise there will be no
                -- space in between. Maybe there is a way to tell blaze not to
                -- eat all spaces.
                " "
                H.button ! A.class_ "btn btn-primary" $ "Add"
    where
        title = printf "Group %s" group :: String


showTaskTemplate :: (Maybe Html) -> E.Task -> [E.Group] -> [E.Group] -> Html
showTaskTemplate maybeMessage task allGroups belongingGroups = template title $ do
    case maybeMessage of
        Just message -> message
        Nothing -> mempty
    H.h1 $ toHtml title
    H.pre ! A.class_ "pre-scrollable" $ toHtml $ E.cmdLine task
    H.div ! A.class_ "row" $ do
        H.div ! A.class_ "col-md-6" $ do
            H.p "Here are the groups this task belongs to:"
            H.ul $ forM_ belongingGroups groupToLi
        H.div ! A.class_ "col-md-6" $ do
            showForm allGroups belongingGroups
    where
        title = printf "Task %d" $ E.pid task :: String
        showForm :: [String] -> [String] -> Html
        showForm allGroups belongingGroups = do
            H.form ! A.method "POST" ! A.class_ "form-inline" $ H.fieldset $ do
                H.legend "Add this task to a group"
                H.select ! A.class_ "form-control chosen-select"
                        ! H.dataAttribute "placeholder" "Choose a Group..."
                        ! A.name "group" $ do
                    groupToOption ""
                    forM_ (allGroups \\ belongingGroups) groupToOption
                -- XXX: Silly space again
                " "
                H.button ! A.class_ "btn btn-primary" $ "Add"
        groupToOption :: String -> Html
        groupToOption group = H.option ! A.value (toValue group) $ toHtml group


template :: String -> Html -> Html
template title body = H.docTypeHtml ! A.lang "en" $ do
    H.head $ do
        H.meta ! A.charset "utf-8"
        H.title $ toHtml title
        H.link ! A.href "/css/bootstrap.min.css" ! A.rel "stylesheet"
        H.link ! A.href "/css/chosen.min.css" ! A.rel "stylesheet"
        H.link ! A.href "/css/chosen-bootstrap.css" ! A.rel "stylesheet"
        H.link ! A.href "/css/cmanager.css" ! A.rel "stylesheet"
    H.body $ do
        H.div ! A.class_ "navbar navbar-inverse navbar-fixed-top" $ H.div ! A.class_ "container" $ do
            H.div ! A.class_ "navbar-header" $ do
                H.a ! A.class_ "navbar-brand" ! A.href "/" $ "CManager"
        H.div ! A.class_ "container" $ do
            body
        H.script ! A.src "/js/jquery.min.js" $ mempty
        H.script ! A.src "/js/bootstrap.min.js" $ mempty
        H.script ! A.src "/js/chosen.jquery.min.js" $ mempty
        H.script ! A.src "/js/cmanager.js" $ mempty


groupToLi :: String -> Html
groupToLi group = H.li $
    H.a ! A.href (toValue $ "/groups" </> urlEncode group) $ toHtml group


taskToLi :: E.Task -> Html
taskToLi task =
    H.li $ H.a ! A.href (toValue $ "/tasks" </> show pid) $ toHtml pid
    where pid = E.pid task


alert :: String -> Html -> Html
alert type_ message = H.div ! A.class_ (toValue class_) $ do
    H.button
        ! A.type_ "button"
        ! A.class_ "close"
        ! H.dataAttribute "dismiss" "alert"
        ! H.customAttribute "aria-hidden" "true"
        $ "×"
    message
    where
        class_ = printf "alert alert-%s alert-dismissable" type_ :: String
