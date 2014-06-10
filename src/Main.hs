{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where


import qualified Happstack.Lite as L
import Happstack.Lite (dir, nullDir, msum, method, Method (GET, POST))

import qualified Web as W
import qualified Api as A


main :: IO ()
main = do
    putStrLn "Running on port 8000"
    L.serve Nothing myApp


-- TODO: the home page is 404
myApp :: L.ServerPart L.Response
myApp = msum
  [ dir "groups" $ nullDir >> W.listGroups
  , dir "groups" $ msum [ method GET >> W.showGroup Nothing
                        , method POST >> W.shimGroup >>= W.showGroup
                        ]
  , dir "tasks"  $ msum [ method GET >> W.showTask Nothing
                        , method POST >> W.shimTask >>= W.showTask
                        ]
  , dir "api" $ dir "groups" $ nullDir >> A.listGroups
  , dir "api" $ dir "groups" $ msum [ method GET >> A.showGroup
                                    , method POST >> A.addTaskToGroup
                                    ]
  , dir "api" $ dir "tasks"  $ A.showTask
  ]
