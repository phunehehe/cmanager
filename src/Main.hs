{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where


import qualified Happstack.Lite as L
import Happstack.Lite (dir, nullDir, msum, method)

import qualified Web as W
import qualified Api as A


main :: IO ()
main = do
    putStrLn "Running on port 8000"
    L.serve Nothing myApp


myApp :: L.ServerPart L.Response
myApp = msum
  [ dir "groups" $ nullDir >> W.listGroups
  , dir "groups" $ W.showGroup
  , dir "tasks"  $ W.showTask
  , dir "api" $ dir "groups" $ nullDir >> A.listGroups
  , dir "api" $ dir "groups" $ msum [ method L.GET >> A.showGroup
                                    , method L.POST >> A.addTaskToGroup
                                    ]
  , dir "api" $ dir "tasks"  $ A.showTask
  ]
