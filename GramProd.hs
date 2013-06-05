{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | The @GramProd@ executable reads a grammatical description (from stdin or a
-- file) and produces a set of grammars, each written into a separate file.
--
-- It is possible to both, produce @LaTeX@ and @Haskell@ output. The Haskell
-- grammars require "ADPfusion" to be useful -- and you have to provide
-- algebras that actually evaluate parses.

module Main where

import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Semigroup
import qualified Text.LaTeX.Base.Render as Latex
import System.Console.CmdArgs hiding (def)
import System.IO
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>))
import Text.Printf
import Text.Trifecta
import Text.Trifecta.Delta

import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.LaTeX
import BioInf.GrammarProducts.Haskell
import BioInf.GrammarProducts.Parser



main :: IO ()
main = do
  o <- cmdArgs $ modes [optionLatex, optionHaskell]
  let g = runGrammarLang $ flip evalStateT def $ parseDesc
  r <- case infile o of
    "" -> getContents >>= return . parseString g (Directed "stdin" 0 0 0 0)
    fn -> parseFromFileEx g fn
  case r of
    Failure e -> liftIO $ displayIO stdout $ renderPretty 0.8 80 $ e <> linebreak
    Success (gs,ps) -> case o of
      Latex{..} -> do
        let latex g = Latex.renderFile (printf "%s/%s.tex" outdir (g^.gname)) . renderGrammarLaTeX columns $ g
        when withatoms $ mapM_ latex gs
        mapM_ latex ps
      Haskell{..} -> do
        let s = renderGrammarHaskell (if withatoms then gs else [] ++ ps)
        -- writeFile (printf "%s/%s.hs" outdir (g^.gname)) s
        putStrLn s

data Options
  = Latex
    { infile    :: String
    , outdir    :: String
    , withatoms :: Bool
    , columns   :: Int
    }
  | Haskell
    { infile    :: String
--    , outdir    :: String
    , withatoms :: Bool
    }
  deriving (Show,Data,Typeable)

optionLatex = Latex
  { infile    = ""    &= help "grammar file to read (stdin if not given)"
  , outdir    = "."   &= help "directory to put grammars in (./ if not given)"
  , withatoms = False &= help "if set, source grammars (atoms) are written to target, too"
  , columns   = 1     &= help "align grammar to 1 or 2 columns?"
  }

optionHaskell = Haskell
  {
  }

