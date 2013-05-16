
module BioInf.GrammarProducts where

import Text.Trifecta (parseFromFile)
import Control.Monad.Trans.State.Strict
import Data.Default
import Text.LaTeX.Base.Syntax (LaTeX)
import Text.LaTeX.Base.Render (render,renderFile,renderAppend)
import qualified Data.Text.IO as T
import Data.Monoid

import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.Parser
import BioInf.GrammarProducts.Direct
import BioInf.GrammarProducts.LaTeX

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
