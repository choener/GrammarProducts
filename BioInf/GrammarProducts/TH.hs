{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
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
        TH.reportWarning $ unlines $ [ ""
                                     , "have successfully constructed product ..."
                                     , ""
                                     , "terminals:"
                                     ] ++ (map show (terminals c)) ++ ["", "non-terminals:"]
                                     ++ (map show (nonterms c)) ++ ["", "rules:"]
                                     ++ (map show (rules c))
        cg <- grammarDecQ c
        return
          [ cg
          ]
        {-
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
          -}

sanitize :: String -> String
sanitize = f . filter isAlphaNum where
  f [] = []
  f (x:xs) = toLower x : xs

-- | Create a grammar declaration from a 'Grammar' object
--
-- TODO add INLINE pragma!

grammarDecQ :: Grammar -> DecQ
grammarDecQ g@Grammar{..} = do
  let gname = mkName $ "g" ++ name
  -- creating a lookup storage for 'Grammar' function names to TH 'newName's.
  -- We don't want to inadvertantly capture something from the outside
  fnames <- sequence [ newName (sanitize $ show f) >>= \z -> return (f,z) | f <- functions ]
  TH.reportWarning $ show fnames
  -- non-terminal names
  nnames <- sequence [ newName (sanitize $ show n) >>= \z -> return (n,z) | n <- nonterms ]
  TH.reportWarning $ show nnames
  -- all the terminal names, each dimension is bound separately
  let tbs = terminalBinders terminals
  tnames <- sequence [ newName (sanitize $ show t) >>= \z -> return (t,z) | t <- tbs ]
  TH.reportWarning $ unlines $ ["","bound terminal names:"] ++ map show tnames
--  blargs <- mkTermCtor $ tnames
--  TH.reportWarning $ show $ blargs
  -- arguments to capture: (i) functions to apply, (ii) non-terminals (actually the memoization data structure), (iii) terminal data (NOT terminal symbol, those are created here)
  let args = [ -- tupP (map (varP . snd) fnames) -- (i) all functions are now bound
--             , tupP (map (varP . snd) nnames)
--             , tupP (map (varP . snd) tnames)
             ]
  let gbody = tupE [] -- $ rulesToTupleQ g fnames nnames tnames
  -- the expression (capture arguments and the resulting RHS)
  let e = lamE args gbody
  -- the outermost part in creating the grammar
  -- valD (g::Pat) (b::Body) (ds::[Dec]) ==>
  -- g = b where ds
  g <- valD (varP gname) (normalB e) []
  return g

-- | create one ADPfusion rule.

mkRule :: Names -> Rule -> ExpQ
mkRule Names{..} (Rule lhs f rhs) = do
  let stack = let go :: ExpQ -> ExpQ -> ExpQ
                  go g r = infixE (Just g) (conE $ mkName "%") (Just r)
              in  foldl1' go $ map (vsymToExpQ M.!) rhs
  infixE (Just $ vfunToExpQ M.! f) (conE $ mkName "<<<") (Just $ stack)

-- | Join different rules by using @(|||)@ and @(...)@ providing a complete
-- right-part for a grammar non-terminal.

mkADPproductions :: Names -> [Rule] -> VFun -> ExpQ
mkADPproductions Names{..} rules h = do
  undefined

-- | Creates the 'Names' object. We want to be able to take, say, a 'Rule' and
-- easily construct the ADPfusion representation. For this we need access to
-- the 'Name' associated with symbols and other things.

mkNames :: Grammar -> Q Names
mkNames = undefined

data Names = Names
  { vfunToExpQ :: M.Map VFun ExpQ
  , vsymToExpQ :: M.Map VSym ExpQ
  }

rulesToTupleQ :: Grammar -> [(VFun,Name)] -> [(VSym,Name)] -> [((String,Int),Name)] -> [ExpQ]
rulesToTupleQ g@Grammar{..} fn nn tn = map ruleGen $ groupBy ((==) `on` _lhs) $ rules where
  ruleGen [] = error $ "empty rules in grammar: " ++ show g
  ruleGen rs@(r:_) = tupE [theNT r, theProductions rs]
  theNT r
    | Just n <- lookup (_lhs r) nn = varE n -- TODO bound to actual nam, NOT wrapped in 'MTable' for now!!!
  theProductions rs = tupE [] --undefined

-- | Build a terminal constructor
--
-- TODO This function should later be generalized and given a set of
-- constructor names to use (meaning that in the grammar description we want to
-- tag each terminal with the terminal type we would like to use).

mkTermCtor :: [((String,Int),Name)] -> VSym -> ExpQ
mkTermCtor ts (VSym xs) = do
  TH.reportWarning $ show ts
  let t = foldl' go (conE $ mkName "T") (zip xs [0..]) -- (map (conE . snd) ts)
  appE (conE $ mkName "Term") $ t
  where
    go :: ExpQ -> (Sym, Int) -> ExpQ
    go x (Sym T s,k)
      | Just n <- lookup (s,k) ts
      , Just trm <- lookup s trmCtors = infixE (Just x) (conE $ mkName ":.") (Just trm)
      | otherwise = error $ "can't build TermCtor for: " ++ show (VSym xs) -- = infixE (Just x) (conE $ mkName ":.") (Just n)
    trmCtors = [ ("p", conE $ mkName "Peek")
               , ("a", conE $ mkName "Chr")
               , ("-", conE $ mkName "Deletion")    -- TODO need to write "Deletion" data constructor in ADPfusion
               ]

-- | Associate terminal names in VSym's with a "dimension" and the name itself,
-- thereby returning the list of unique binders.
--
-- Example: T:a.a.a becomes (T:a.a.a, (a_0,a_1,a_2)) and the unique binders are
-- [a_0,a_1,a_2].

terminalBinders :: [VSym] -> [(String,Int)]
terminalBinders = {- filter (("-"/=).fst) . -} nub . concat . map terminalBinder where

  -- associate each terminal symbol with a dimension

terminalBinder (VSym ts) = zip (map _n ts) [0..]

-- | Folds different grammars using the grammar product operation.
--
-- NOTE the @delete@ operation used here should be employed last as it works on
-- the fully constructed grammar and removes "every" (!) rule that contains the
-- specified terminal symbol. If you need very speciliased treatment, write
-- your own version of 'foldGrammars' and give that to the TH system.

foldGrammars :: GrammarOperations -> [Grammar] -> GProduct -> Grammar
foldGrammars gOps gs p = rename $ addRules $ delete $ initial $ pprod p where
  rename g = g {name = pname p}
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
  addRules newG = newG{ rules = rules newG ++ prules p}

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
