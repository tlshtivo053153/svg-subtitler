module Main (main) where

import Option
import Subtitle

import Diagrams.Backend.CmdLine

main :: IO ()
main = mainWith (MyCustom subtitler)
