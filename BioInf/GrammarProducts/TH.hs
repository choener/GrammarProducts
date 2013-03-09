{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- | The rationale behind the grammar product quasi quoting system is to
-- provide the correct terminal and non-terminal symbols to the quoter. Then
-- the user only needs to provide the input to the single-dimensional grammar,
-- instead of having to give lots and lots of constructs like @Term (T:.Chr
-- a:.Chr b)@, we just need to give @a@ and @b@.
--
-- This, of course, reduces the generality of the grammars, but makes it much
-- easier to work with larger products.

module BioInf.GrammarProducts.TH where

import Data.ByteString.Char8 (pack)
import Data.Semigroup
import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH
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

import BioInf.GrammarProducts.Parser
import BioInf.GrammarProducts.Grammar


-- ** QuasiQuoters

type ProdOp = Grammar -> Grammar -> Grammar

-- | The user fills the 'GrammarOperations' data type with operations,
-- terminal, and non-terminal symbols which are then used by the quasi-quoting
-- device to generate the correct grammars.

data GrammarOperations = GrammarOperations
  { productOperations :: [(String,ProdOp)]
  }

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
  case parseString
        pFullDesc -- pProduct --pGrammar
        (Directed (pack fname) (fromIntegral lpos) 0 0 0)
        (trim s)
    of Failure f -> do
        TH.runIO $ displayIO stdout $ renderPretty 0.8 80 $ f <> linebreak
        fail ""
       Success (gs,p) -> do
        mapM_ (\g -> TH.reportWarning $ unlines ["","have successfully parsed grammar object ...", show g]) gs
        TH.reportWarning $ unlines ["","have successfully parsed product object ...", show p]
        -- at this point we have at least one grammar and exactly one product description
        let c = foldGrammars gOps gs p
        TH.reportWarning $ unlines [ ""
                                   , "have successfully constructed product ..."
                                   , show c
                                   , "number of rules: " ++ show (length $ rules c)
                                   ]
        bla <- TH.runQ [d| ggggg a b c = (a,b,c) |]
        let gname = TH.mkName $ "g" -- ++ name s
        -- TODO Baustelle:
        -- momentan erzeuge ich ein Objekt "gTest1 :: ()"
        let g = TH.ValD (TH.VarP gname)
                  (TH.NormalB
                    (TH.TupE []
                    )
                  ) []
        -- make me inlineable
        let inl = TH.PragmaD $ TH.InlineP gname TH.Inline TH.FunLike TH.AllPhases
        return $
          [ g   -- write the grammar itself
          , inl -- inline the grammar
          ] ++ bla

-- | Folds different grammars using the grammar product operation.
--
-- NOTE the @delete@ operation used here should be employed last as it works on
-- the fully constructed grammar and removes "every" (!) rule that contains the
-- specified terminal symbol. If you need very speciliased treatment, write
-- your own version of 'foldGrammars' and give that to the TH system.

foldGrammars :: GrammarOperations -> [Grammar] -> GProduct -> Grammar
foldGrammars gOps gs p = delete $ initial $ pprod p where
  delete g = g {rules = filter (\(Rule l f rs) -> null $ intersect (pdels p) rs) $ rules g}
  initial []  = error $ "specified empty product: " ++ show (pprod p)
  initial (ProdGr gn : ps)
    | Just g <- find ((gn==).name) gs = go g ps
    | otherwise = error $ "specified non-existant grammar: " ++ show (gs,p)
  go g [] = g
  go g1 (ProdOp o : ProdGr gn : ps)
    | Just g2 <- find ((gn==).name) gs
    , Just f <- lookup o (productOperations gOps)
    = go (f g1 g2) ps
  go _ (ProdGr g : _) = error $ "wrong order in product term: " ++ show p

-- *** grammar-structure prettyprinter QQ

-- | QuasiQuoter parsing the input, producing the 'Grammar' ctor, but otherwise
-- generating a string for pretty-printing.

qqGV :: QuasiQuoter
qqGV = QuasiQuoter
  { quoteExp = qqParseExpGV
  , quotePat = error "patterns not useful for grammar QQs"
  , quoteType = error "types not useful for grammar QQs"
  , quoteDec = error "declarations not useful for grammar QQs"
  }

qqParseExpGV :: String -> TH.ExpQ
qqParseExpGV s = do
  loc <- TH.location
  let trim ('\n':xs) = xs
      trim xs        = xs
  let fname = TH.loc_filename loc
  let (lpos,cpos) = TH.loc_start loc
  let fs = case parseString
                  pGrammar
                  (Directed (pack fname) (fromIntegral lpos) 0 0 0)
                  (trim s)
            of Success s -> s
               Failure f -> error $ show f
               --error $ show $ unsafePerformIO $ displayIO stdout $ renderPretty 0.8 80 $ f <> linebreak

  return $ TH.LitE $ TH.StringL $ show $ fs

-- *** verbatim QQ

-- | QuasiQuoter producing the input verbatim as a string for prettyprinting.

qqV :: QuasiQuoter
qqV = QuasiQuoter
  { quoteExp = qqParseExpV
  , quotePat = error "patterns not useful for grammar QQs"
  , quoteType = error "types not useful for grammar QQs"
  , quoteDec = error "declarations not useful for grammar QQs"
  }

-- | Returns the input verbatim

qqParseExpV :: String -> TH.ExpQ
qqParseExpV s = do
  loc <- TH.location
  let fname = TH.loc_filename loc
  let lpos  = TH.loc_start    loc
  return $ TH.LitE $ TH.StringL s





















{-
-- Describe a format string
data Format = D | S | L String

-- Parse a format string.  This is left largely to you
-- as we are here interested in building our first ever
-- Template Haskell program and not in building printf.
parse :: String -> [Format]
parse "x" = [D]
parse s   = [ L s ]

-- Generate Haskell source code from a parsed representation
-- of the format string.  This code will be spliced into
-- the module which calls "pr", at compile time.
gen :: [Format] -> TH.Q TH.Exp
gen [D]   = [| \n -> show n |]
gen [S]   = [| \s -> s |]
gen [L s] = TH.stringE s

-- Here we generate the Haskell code for the splice
-- from an input format string.
pr :: String -> TH.Q TH.Exp
pr s = gen (parse s)
-}
