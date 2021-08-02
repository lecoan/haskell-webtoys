{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import App (app)
import BasicPrelude (IO)
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3000 app