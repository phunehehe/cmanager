{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where


import qualified Web as W
import Happstack.Lite (dir, nullDir, serve, ServerPart, Response, msum)


main :: IO ()
main = do
    putStrLn "Running on port 8000"
    serve Nothing myApp


myApp :: ServerPart Response
myApp = msum
  [ dir "groups" $ nullDir >> W.listGroups
  , dir "groups" $ W.showGroup
  , dir "tasks"  $ W.showTask
  ]
