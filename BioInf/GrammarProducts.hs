{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE GADTs #-}

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

import Language.Haskell.TH.Quote
import qualified Language.Haskell.TH as TH
import Text.Trifecta
import Control.Applicative
import Data.Semigroup
import qualified Data.Set as S
import Data.Set (Set(..))
import GHC.TypeLits
import Text.Printf
import Data.Foldable as F
import Control.Applicative
import Control.Lens
import Text.Trifecta.Result
import Text.PrettyPrint.ANSI.Leijen as Pretty hiding (line, (<>), (<$>), empty, string, char)
import System.IO (stdout)
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad (liftM)
import qualified Data.Vector as V
import Data.Vector (Vector (..))



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

-- | A left-hand side in a grammar

newtype Lhs = Lhs [Sym] -- cfg's and simpler have [SymN] here!

-- | higher-dimensional lhs

newtype LhsD = LhsD (Vector Lhs)

-- | RHS

newtype Rhs = Rhs [Sym]

newtype RhsD = RhsD (Vector Rhs)

-- | full production rule: one or more right-hand sides

data PR = PR LhsD [RhsD]

-- | full grammar

data Grammar = Grammar
  {
  }

-- ** QuasiQuoters

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
  -- baustelle
  return $ TH.LitE $ TH.StringL s

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

-- ** old

{-

symN1 x = [SymN x]
symT1 x = [SymT x]

-- data ProRHS = ProRHS { _fun :: [Fun], _terms :: [[Sym]] }

-- | Define a product to be of vectorial non-terminal to a set of right-hand
-- sides. Each side is a list of terminals and non-terminals. They have the
-- same dimensionality as the left-hand side. In each list "column" can be
-- terminals and non-terminals.
--
-- NOTE We currently do not enforce same dimensionality. Would be nice but
-- TypeLits are not yet ready ...
--
-- TODO add attribute function (that is required to evaluate the RHS

data Pro = Pro { _lh :: [Sym], _rhs :: (Set [[Sym]]) }
  deriving (Eq,Ord,Show)

data Grammar = Grammar
  { _n :: Set [Sym]
  , _t :: Set [Sym]
  , _p :: Set Pro
  }
  deriving (Eq,Ord,Show)

makeLenses ''Grammar

-- | Direct product of two grammars.

productG :: Grammar -> Grammar -> Grammar
productG (Grammar ns1 ts1 ps1) (Grammar ns2 ts2 ps2) = Grammar (productNT ns1 ns2) (productNT ts1 ts2) (productP ps1 ps2)

-- | Direct product of two terminal sets.

productNT :: Set [Sym] -> Set [Sym] -> Set [Sym]
productNT xs ys = S.fromList [ x++y | x <- S.toList xs, y <- S.toList ys ]

productP :: Set Pro -> Set Pro -> Set Pro
productP xs ys = S.fromList [ combine x y | x <- S.toList xs, y <- S.toList ys ] where
  combine (Pro x xs) (Pro y ys) = Pro (x++y) (S.fromList [ cmb a b | a<- S.toList xs, b <- S.toList ys ])
  cmb = undefined



-- * trifecta parser for grammars

-- | Simple test case.
--
-- N: X Y
-- T: m d
--
-- X -> X m | Y m
-- Y -> X d | Y d

test = case parseString pGrammar mempty "N: X Y\nT: m d\n\nX -> X m | Y m\nY -> X d | Y d" of
         Success s -> s
         Failure f -> error $ show $ unsafePerformIO $ displayIO stdout $ renderPretty 0.8 80 $ f <> linebreak

pGrammar = build <$> pHeader <* some newline <*> sepBy1 pP newline <* eof where
  build (ns,ts) rs = Grammar (S.fromList ns) (S.fromList ts) (S.fromList rs) -- rs needs to (++) combine productions

-- | Each grammar declares both non-terminals and terminals first

pHeader = (,) <$> pn <* newline <*> pt where
  pn = string "N:" *> ws *> (sepBy1 pN ws)
  pt = string "T:" *> ws *> (sepBy1 pT ws)

pP = f <$> pN <* string " -> " <*> sepBy1 rhs (char '|' <* ws) where
  f n rs = Pro n (S.fromList rs)
  rhs = sepEndBy1 (pN <|> pT) ws

pN = symN1 <$> some upper
pT = symT1 <$> some lower

ws = some $ char ' '
-}

