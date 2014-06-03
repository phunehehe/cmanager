{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- XXX: This is said to be unportable
import GHC.IO.Exception (IOException (IOError))
import Control.Monad (forM_, guard)
import Control.Monad.Trans (liftIO)
import Data.List ((\\))
import Data.Monoid (mempty)
import Data.Text.Lazy (unpack)
import Happstack.Lite (
    dir, nullDir, serve, ServerPart, Response, msum, toResponse, path, ok,
    Method (POST), lookText)
import Happstack.Server (askRq, rqUri, rqMethod, matchMethod)
import System.IO (hPutStrLn, stderr)
import System.IO.Error (tryIOError)
import Text.Blaze.Html5 ((!), toHtml, toValue, Html)
import Text.Printf (printf)

import Helpers (
    getAllGroups, getTasksOfGroup, getCmdLine, getGroupsOfTask,
    groupExists, taskExists, readInt, addTaskToGroup)
import Templates(template, groupToLi, taskToLi, alert)


main :: IO ()
main = do
    putStrLn "Running on port 8000"
    serve Nothing myApp


myApp :: ServerPart Response
myApp = msum
  [ dir "groups" $ nullDir >> listGroups
  , dir "groups" $ showGroup
  , dir "tasks"  $ showTask
  ]


listGroups :: ServerPart Response
listGroups = do
    groups <- liftIO getAllGroups
    rq <- askRq
    ok $ toResponse $ template (rqUri rq) "Groups" $ do
        H.h1 "All Groups"
        H.ul $ forM_ groups groupToLi


tryAddTaskToGroup :: Integer -> String -> IO Html
tryAddTaskToGroup pid group = do
    result <- liftIO $ tryIOError $ addTaskToGroup pid group
    case result of
        Left error -> do
            hPutStrLn stderr $ show error
            return $ alert "danger" $ do
                "Something was wrong when adding task "
                pidToHtml pid
                " to group "
                groupToHtml group
                ":"
                H.ul $ H.li $ userMessage error
        Right _ -> return $ alert "success" $ do
            "Task "
            pidToHtml pid
            " has been added to group "
            groupToHtml group
            "."
    where
        groupToHtml :: String -> Html
        groupToHtml group = H.strong $ toHtml group

        pidToHtml :: Integer -> Html
        pidToHtml pid = H.strong $ toHtml pid

        userMessage :: IOError -> Html
        userMessage (IOError _ _ _ description _ _) = toHtml description


showGroup :: ServerPart Response
showGroup = do
    path $ \(group :: String) -> do
        guard =<< liftIO (groupExists group)
        rq <- askRq
        postResult <- if matchMethod POST $ rqMethod rq
            then do
                pidText <- lookText "pid"
                -- TODO: validate pid
                let pid = readInt $ unpack pidText
                liftIO $ tryAddTaskToGroup pid group
            else return mempty
        tasks <- liftIO $ getTasksOfGroup group
        ok $ toResponse $ template (rqUri rq) ("Group " ++ group) $ do
            postResult
            H.h1 $ toHtml $ "Group " ++ group
            H.div ! A.class_ "row" $ do
                H.div ! A.class_ "col-md-5" $ do
                    H.p "Here are the tasks in this group:"
                    H.ul $ forM_ tasks taskToLi
                H.div ! A.class_ "col-md-5" $ do
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


showTask :: ServerPart Response
showTask = do
    path $ \(pid :: Integer) -> do
        guard =<< (liftIO $ taskExists pid)
        rq <- askRq
        postResult <- if matchMethod POST $ rqMethod rq
            then do
                groupText <- lookText "group"
                -- TODO: validate group
                let group = unpack groupText
                liftIO $ tryAddTaskToGroup pid group
            else return mempty
        cmdLine <- liftIO $ getCmdLine pid
        allGroups <- liftIO $ getAllGroups
        belongingGroups <- liftIO $ getGroupsOfTask pid
        ok $ toResponse $ template (rqUri rq) ("Task " ++ show pid) $ do
            postResult
            H.h1 $ toHtml $ (printf "Task %d: %s" pid cmdLine :: String)
            H.div ! A.class_ "row" $ do
                H.div ! A.class_ "col-md-5" $ do
                    H.p "Here are the groups this task belongs to:"
                    H.ul $ forM_ belongingGroups groupToLi
                H.div ! A.class_ "col-md-5" $ do
                    showForm allGroups belongingGroups
    where
        showForm :: [String] -> [String] -> Html
        showForm allGroups belongingGroups = do
            H.form ! A.method "POST" ! A.class_ "form-inline" $ H.fieldset $ do
                H.legend "Add this task to a group"
                H.select ! A.class_ "form-control"
                        ! A.id "group" ! A.name "group" $ do
                    forM_ (allGroups \\ belongingGroups) groupToOption
                -- XXX: Silly space again
                " "
                H.button ! A.class_ "btn btn-primary" $ "Add"

        groupToOption :: String -> Html
        groupToOption group = H.option ! A.value (toValue group) $ toHtml group
