
module BioInf.GrammarProducts where

import Control.Monad.Trans.State.Strict
import Data.Default
import Data.Monoid
import qualified Data.Text.IO as T
import Text.LaTeX.Base.Render (render,renderFile,renderAppend)
import Text.LaTeX.Base.Syntax (LaTeX)
import Text.Trifecta (parseFromFile)

import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.LaTeX
import BioInf.GrammarProducts.Haskell
import BioInf.GrammarProducts.Op.Add
import BioInf.GrammarProducts.Op.Direct
import BioInf.GrammarProducts.Op.Power
import BioInf.GrammarProducts.Op.Subtract
import BioInf.GrammarProducts.Parser

testHS :: IO ()
testHS = do
  pff <- parseFromFile (runGrammarLang $ flip evalStateT def $ parseDesc) "./tests/nway.gra"
  case pff of
    Nothing -> return ()
    Just (gs,ps) -> do
      mapM_ (putStrLn . rgh) ps
{-
test :: IO ()
test = do
  pff <- parseFromFile (runGrammarLang $ flip evalStateT def $ parseDesc) "./tests/protein.gra"
  case pff of
    Nothing -> return ()
    Just (gs,ps) -> do
      print gs
      print ps
      -- mapM_ (\g -> rg g >> putStrLn "") (gs ++ ps)
      renderFile "../Paper-GrammarProducts/tmp.tex" (mconcat $ map rgt $ gs ++ ps) -- (renderAppend (map renderGrammarLaTeX $ gs++ps))

rgt = renderGrammarLaTeX

rg :: Grammar -> IO ()
rg = T.putStrLn . render . (renderGrammar :: Grammar -> LaTeX)
-}
