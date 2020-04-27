module Main where

import Lib
import System.Directory
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

api = ".config/Custom-E-liter-4k-is-best"

main :: IO ()
main = getHomeDirectory >>= \home -> TIO.readFile (home ++ api) 
                        >>= \bot -> tweet (T.pack "リッカスはいいぞ") (T.empty) ((Prelude.map T.unpack.T.lines) bot) >> return()
