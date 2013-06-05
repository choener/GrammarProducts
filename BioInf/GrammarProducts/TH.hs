{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module BioInf.GrammarProducts.TH where

import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.ByteString.Char8 (pack)
import Data.Semigroup
import Language.Haskell.TH.Quote
import Language.Haskell.TH as TH
import System.IO (stdout)
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty, string, char)
import Text.Printf
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Trifecta.Result
import Control.Lens
import Data.Data
import Data.Data.Lens
import Data.Typeable
import Data.List
import Data.Char
import Data.Function
import Data.Foldable (foldlM)
import qualified Data.Map as M
import Control.Monad.Trans.State.Strict
import Data.Default
import Language.Haskell.TH.Build
import Language.Haskell.Meta hiding (parseDecs)
import Language.Haskell.Meta.Parse.Careful
import Language.Haskell.Exts.Extension
import qualified Language.Haskell.Exts.Syntax as Hs
--import Language.Haskell.Parser -- Exts.Annotated
import Language.Haskell.Exts.Parser -- Exts.Annotated
--import Language.Haskell.Exts.Annotated hiding (parseModuleWithMode, ParseMode)


import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.Parser
import BioInf.GrammarProducts.Haskell


type ProdOp = Grammar -> Grammar -> Grammar

data GrammarOperations = GrammarOperations
  { productOperations :: [(String,ProdOp)]
  }

qqGDG = qqGD undefined


qqGD :: GrammarOperations -> QuasiQuoter
qqGD gOps = QuasiQuoter
  { quoteExp = error "" -- qqTesting
  , quotePat = error "patterns not useful for grammar QQs"
  , quoteType = error "types not useful for grammar QQs"
  , quoteDec = qqTesting gOps
  }

qqTesting gOps s = do
  loc <- TH.location
  let trim ('\n':xs) = xs
      trim xs        = xs
  let fname = TH.loc_filename loc
  let (lpos,cpos) = TH.loc_start loc
  let g = runGrammarLang $ flip evalStateT def $ parseDesc
  case parseString
        g
        (Directed (pack fname) (fromIntegral lpos) 0 0 0)
        (trim s)
    of Failure f -> do
        TH.runIO $ displayIO stdout $ renderPretty 0.8 80 $ f <> linebreak
        fail ""
       Success (gs,ps) -> do
        mapM_ (\g -> TH.reportWarning $ unlines ["","have successfully parsed grammar object ...", show g]) gs
        mapM_ (\p -> TH.reportWarning $ unlines ["","have successfully parsed product object ...", show p]) ps
        -- we are interested in emitting code for the products
        g' <- grammarDecQ $ last ps
        return g'

grammarDecQ :: Grammar -> Q [Dec]
grammarDecQ g@Grammar{..} = do
  let ts = parseDecs $ rgh g
  either (\s -> error $ show (rgh g) ++ "\n" ++ s) return ts
  {-
  let ts = parseDecs "f :: Int -> Int\nf x = x+1\ng x = x*2"
  TH.reportWarning $ show ts
  let Right t = ts
  either (error . show) return ts
  -}

{-
parseDecs' :: String -> Either String [Dec]
parseDecs'  = either Left (Right . toDecs) . parseHsDecls'

parseHsDecls' :: String -> Either String [Hs.Decl]
parseHsDecls' = either Left (Right . moduleDecls) . parseResultToEither . parseModuleWithMode myDefaultParseMode'

myDefaultParseMode' :: ParseMode
myDefaultParseMode' = myDefaultParseMode { extensions = myDefaultExtensions' }

myDefaultExtensions' :: [Extension]
myDefaultExtensions' = [ PostfixOperators
                       , QuasiQuotes
                       , UnicodeSyntax
                       , PatternSignatures
                       , MagicHash
                       , ForeignFunctionInterface
                       , TemplateHaskell
                       , RankNTypes
                       , MultiParamTypeClasses
                       , RecursiveDo
                       , RecordWildCards
                       ]
-}

{-
grammarDecQ :: Grammar -> Q [Dec]
grammarDecQ g@Grammar{..} = do
  let snm = "S" ++ (g^.gname)
  --sig <- dataD' () snm ({-m x r-}) [recC' snm [varStrictType' "a" (''Int) ]] ({-names-})
  --return sig
  s <- sigD' "f" (appT' (appT' arrowT ''Bool) ''Bool)
  t <- valD' "f" ('not) ()
  return [s,t]
-}
