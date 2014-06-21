{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where


import qualified Happstack.Lite as L

import Happstack.Lite (ServerPart, dir, msum, method, Method (GET, POST))

import qualified Web as W
import qualified Api as A

import Helpers (Group, Pid)


main :: IO ()
main = do
    putStrLn "Running on port 8000"
    L.serve Nothing myApp


parseGroup :: ServerPart Group
parseGroup = L.path $ \(group :: Group) -> return group


parsePid :: ServerPart Pid
parsePid = L.path $ \(pid :: Pid) -> return pid


myApp :: ServerPart L.Response
myApp = msum
  [ L.nullDir >> W.listGroups
  , dir "groups" $ msum [ method GET >> parseGroup >>= W.showGroup Nothing
                        , method POST >> parseGroup >>= W.processGroup >>= uncurry W.showGroup
                        ]
  , dir "tasks"  $ msum [ method GET >> parsePid >>= W.showTask Nothing
                        , method POST >> parsePid >>= W.processTask >>= uncurry W.showTask
                        ]
  , dir "api" $ dir "groups" $ L.nullDir >> A.listGroups
  , dir "api" $ dir "groups" $ msum [ method GET >> parseGroup >>= A.showGroup
                                    , method POST >> parseGroup >>= A.addTaskToGroup
                                    ]
  , dir "api" $ dir "tasks" $ parsePid >>= A.showTask
  ]
