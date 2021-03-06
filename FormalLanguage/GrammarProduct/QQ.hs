
module FormalLanguage.GrammarProduct.QQ where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Text.Trifecta.Delta (Delta (Directed))
import           Text.Trifecta (parseString)
import           Text.Trifecta.Result (Result (..))
import           Control.Monad.Trans.State.Strict (evalStateT)
import           Data.Default (def)
import           Data.ByteString.Char8 (pack)
import           Control.Lens

import           FormalLanguage.CFG.QQ (trim, parseFormalLanguage)
import           FormalLanguage.CFG.PrettyPrint.ANSI
import           FormalLanguage.CFG.Parser
import           FormalLanguage.CFG.TH
import           FormalLanguage.CFG.Grammar

import FormalLanguage.GrammarProduct.Parser (parseGrammarProduct)



grammarProductF = quoteFile grammarProduct

grammarProduct = QuasiQuoter
  { quoteDec  = parseFormalLanguage parseGrammarProduct
  , quoteExp  = err
  , quotePat  = err
  , quoteType = err
  } where err = error "there is only a Dec quoter"

{-
parseGrammarProduct :: String -> Q [Dec]
parseGrammarProduct s = do
  loc <- location
  let (lpos,cpos) = loc_start loc
  let r = parseString
            ((evalStateT . runGrammarP) P.productParser def)
            (Directed (pack "via QQ") (fromIntegral lpos) 0 0 0)
              $ trim s
  case r of
    (Failure f) -> do
      runIO . printDoc $ f
      fail "aborting parseGrammarProduct"
    (Success g') -> do
      let g = reverse g'
      runIO . mapM_ (printDoc . grammarDoc) $ g
      zs <- thCodeGen $ last g
      return zs
-}

