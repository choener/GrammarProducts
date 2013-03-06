{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
--
-- TODO add a bunch of newtypes to hide implementation details
--
-- TODO replace Set Pro with Map N -> [NT]
--
-- TODO any way to re-introduce statically determined dimensionality? There
-- might be a problem there, say aligning [Xa][Y], but then we want to figure
-- this out before creating the production rule.
--
-- TODO "Grammar -> ADPfusion grammar" with prettyprinting please!
--
-- TODO inlinePrint to print ADPfusion grammar during compilation (just unsafePerformIO that thing?!)
--
-- TODO function that produces specialized algebras for the grammar
--
-- TODO prettyprinting production rules:
-- http://hackage.haskell.org/package/ipprint-0.5
--
-- TODO QQ needs to inline stuff

module BioInf.GrammarProducts where

import Control.Applicative
import Control.Lens
import Control.Monad (liftM)
import qualified Data.Foldable as F
import Data.Semigroup
import Data.Set (Set(..))
import Data.Vector (Vector (..))
import GHC.TypeLits
import Language.Haskell.TH.Quote
import qualified Data.Set as S
import qualified Data.Vector as V
import qualified Language.Haskell.TH as TH
import System.IO (stdout)
import System.IO.Unsafe (unsafePerformIO)
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty, string, char)
import Text.Printf
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Trifecta.Result
import Data.ByteString.Char8 (pack)
import Data.Either



-- * once more, with feeling

-- ** grammar data types

-- | A symbol is a single terminal or non-terminal on either
-- left-hand or right-hand side. This is, on the level of
-- symbol we do not distinguish between LHS and RHS (or
-- equivalently the type of grammar we are looking at).

data Sym
  = SymN { _s :: String }
  | SymT { _s :: String }
  deriving (Eq,Ord,Show)

makeLenses ''Sym

newtype VSym = VSym [Sym]
  deriving (Eq,Ord,Show)

-- | full production rule

data Rule = Rule VSym [VSym]
  deriving (Eq,Ord,Show)

-- | full grammar

data Grammar = Grammar
  { name      :: String
  , terminals :: [VSym]
  , nonterms  :: [VSym]
  , rules     :: [Rule]
  }
  deriving (Eq,Ord,Show)

-- ** QuasiQuoters

qqGD :: QuasiQuoter
qqGD = QuasiQuoter
  { quoteExp = error "" -- qqTesting
  , quotePat = error "patterns not useful for grammar QQs"
  , quoteType = error "types not useful for grammar QQs"
  , quoteDec = qqTesting
  }

--qqParseExpGV :: String -> TH.ExpQ
qqTesting s = do
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
       Success s -> do
        TH.reportWarning $ unlines ["","have successfully parsed grammar object ...", show s]
        -- at this point we have at least one grammar and exactly one product description
        -- TODO Baustelle:
        -- hier muesste man jetzt die Grammatiken multiplizieren nach Regel,
        -- bis man schlussendlich eine durchmultiplizierte Grammatik erhaelt.
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

-- * parsers

-- | parse a single non-terminal

pN :: CharParsing f => f Sym
pN = (SymN .) . (:) <$> upper <*> many alphaNum

-- | parse a single terminal. There is a special case of one or more '-' which
-- we may use too. Theses will probably be bound to a parser that doesn't
-- advance.

pT :: CharParsing f => f Sym
pT =   (SymT .) . (:) <$> lower <*> many alphaNum
   <|> SymT           <$> some (char '-')

-- | parse the list of non-terminal symbols

pNdefn :: CharParsing f => f [Sym]
pNdefn = string "N:" *> ws *> sepBy1 pN ws

-- | parse the list of terminal symbols

pTdefn :: CharParsing f => f [Sym]
pTdefn = string "T:" *> ws *> sepBy1 pT ws

-- | Grammar name

pGrammarName = (:) <$> upper <*> some alphaNum

-- | Parse the first line of the grammar description

pHeader :: CharParsing f => f String
pHeader = oneOf "Gg" *> string "rammar:" *> ws *> pGrammarName

-- | Parse the last line of the grammar description

pLast = string "//"

pRules :: CharParsing f => f Rule
pRules = f <$> pN <* ws <* string "->" <* ws <*> (sepEndBy1 (pN <|> pT) wx) where
  f l rs = Rule (VSym $ [l]) [ VSym [r] | r <- rs ]



-- | Parse a full grammar description

pGrammar = do
  h <- pHeader <* newline
  nts <- sepEndBy1 (Left <$> pNdefn <|> Right <$> pTdefn) newline
  rs <- sepEndBy1 pRules newline
  pLast
  let ns :: [VSym] = map (VSym . (:[])) $ concat $ lefts  nts
  let ts :: [VSym] = map (VSym . (:[])) $ concat $ rights nts
  let g = Grammar
            { name      = h
            , terminals = ts
            , nonterms  = ns
            , rules     = rs
            }
  return g

-- | Parse product operations

data ProdOps
  = ProdGr String
  | ProdOp String
  deriving (Eq,Ord,Show)

pProductOps = sepEndBy (pGr <|> pOp) ws where -- (:) <$> pGr <*> many (pOp <*> pGr) where
  pGr = ProdGr <$> pGrammarName
  pOp = ProdOp <$> string "*"

-- | parse what we maybe want to delete

pDelete = string "remove:" *> ws *> some pR where
  pR = sepBy1 pT (char ',')

-- | Parse a product description

pProduct = do
  n <- (:) <$ string "Product:" <* ws <*> letter <*> some alphaNum <* newline
  p <- string "Prod:" *> ws *> pProductOps <* newline
  rs <- sepEndBy1 pDelete newline
  pLast
  return (n,p,rs)

pFullDesc = do
  gs <- sepEndBy1 pGrammar newline <?> "need to define at least one grammar!"
  p  <- pProduct <?> "need to define exactly one product description"
  spaces
  eof
  return (gs,p)

-- | helper function with 1 .. space characters

ws = some $ char ' '
wx = many $ char ' '

-- ** old


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
