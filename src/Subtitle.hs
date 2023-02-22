module Subtitle
  ( subtitler
  ) where

import Data.Char (isAscii)
import qualified Data.List as L

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

import Graphics.SVGFonts.ReadFont (PreparedFont)
import qualified Graphics.SVGFonts as F

subtitleText :: F.TextOpts Double -> String -> Diagram B
subtitleText opts = d'
  where
    d :: String -> Diagram B
    d s = F.drop_rect (F.fit_height 0.75 $ F.svgText opts s) # stroke
    d' :: String -> Diagram B
    d' s = ((d s # fc white # lc white # lw 1)
            <> (d s # lc black # lw 4)) # center

subtitler :: PreparedFont Double -> String -> Diagram B
subtitler f t = do
  let
    opts = F.TextOpts f F.KERN False
    textLength = lengthJP t
    (t1, t2, t3) = if textLength > (56*3)
                      then splitJP3 t
                      else case splitJP 56 t of
                             [] -> ("","","")
                             [xs] -> (xs,"","")
                             [xs,ys] -> (xs,ys,"")
                             [xs,ys,zs] -> (xs,ys,zs)
                             [xs,ys,zs,ws] -> (xs,ys,zs++ws)
                             _ -> ("","","")
    diagrams :: [Diagram B]
    diagrams = map (subtitleText opts) [ t1, t2, t3 ]
    diagram :: Diagram B
    diagram = let padDiagram = (strutY 0.25 === vsep 0.25 diagrams === strutY 0.25)
               in strutX 0.25 ||| padDiagram ||| strutX 0.25
  let w = 16 / width (boundingRect diagram :: Diagram B)
      diagram' = if w < 1 then diagram # scale w
                          else diagram
  rect 16 6.75 === diagram' # center

-- nonascii 28 letters == ascii 56 letters
charSizeJP :: Char -> Int
charSizeJP c | isAscii c = 1
             | otherwise = 2

lengthJP :: String -> Int
lengthJP = sum . map charSizeJP

-- splitJP 10 "hello world" == ["hello worl", "d"]
-- splitJP 10 "あああああいいいいい" == ["あああああ", "いいいいい"]
splitJP :: Int -> String -> [String]
splitJP n = go 
  where
    go [] = []
    go str =
      let n' = last . L.findIndices (<= n) . scanl (+) 0 $ map charSizeJP str
          (xs, ys) = splitAt n' str
       in xs : go ys

splitJP3 :: String -> (String, String, String)
splitJP3 s =
  let s' = go s
   in case s' of
        [xs,ys,zs] -> (xs,ys,zs)
        [xs,ys,zs,ws] -> (xs,ys,zs++ws)
        _ -> ("","","")
  where
    n = sum $ map charSizeJP s
    go [] = []
    go str =
      let n' = last . L.findIndices (<= div n 3) . scanl (+) 0 $ map charSizeJP str
          (xs, ys) = splitAt n' str
       in xs : go ys
