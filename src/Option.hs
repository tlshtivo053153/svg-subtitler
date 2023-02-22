{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
module Option
  ( MyCustom(..)
  ) where

import Diagrams.Backend.CmdLine
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude             hiding ((<>))
import Options.Applicative
import Control.Monad (zipWithM_)

import qualified Graphics.SVGFonts as F
import Graphics.SVGFonts.ReadFont (PreparedFont)

newtype MyCustom d = MyCustom d

data MyCustomOpts = MyCustomOpts FilePath [FilePath]

instance Parseable MyCustomOpts where
  parser = MyCustomOpts
      <$> strOption ( short 'f'
                    <> long "font"
                    <> metavar "FILE_PATH"
                    <> help "Font file (.svg)"
                    )
      <*> many (strArgument $ metavar "FILE_PATH...")

type MyDiagram = PreparedFont Double -> String -> QDiagram SVG V2 Double Any

instance Mainable (MyCustom MyDiagram) where
  type MainOpts (MyCustom MyDiagram)
        = (MainOpts (QDiagram SVG V2 Double Any), MyCustomOpts)
  mainRender (opts, MyCustomOpts fontPath fs) (MyCustom d) = do
    f <- F.loadFont fontPath
    let ( DiagramOpts w h _, loopOpts, prettyOpts) = opts
        toOpt out = ( DiagramOpts w h out, loopOpts, prettyOpts)
        opts' = map ( toOpt . (++ ".svg") . takeWhile (/= '.')) fs
    fcontents <- mapM readFile fs
    let ds = map (d f) fcontents
    zipWithM_ mainRender opts' ds
