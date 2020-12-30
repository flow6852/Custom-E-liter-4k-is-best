{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import System.Directory
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import System.Environment

api = "/.config/Custom-E-liter-4k-is-best"

main :: IO ()
main = getHomeDirectory >>= \home -> TIO.readFile (home ++ api) 
                        >>= \bot -> getArgs
                        >>= \args -> case args of
                                        [] -> tweet "リッカスはいいぞ" (T.empty) ((Prelude.map T.unpack.T.lines) bot) >> return()
                                        [user, id] -> tweet (foldl1 T.append [T.singleton '\n', T.pack user, "リッカスはいいぞ"]) (T.pack id) ((Prelude.map T.unpack.T.lines) bot) >> return()

