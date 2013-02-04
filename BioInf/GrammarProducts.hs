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
-- TODO inlinePrint to print ADPfusion grammar during compilation
--
-- TODO function that produces specialized algebras for the grammar

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



data Sym
  = SymN { _s :: String }
  | SymT { _s :: String }
  deriving (Eq,Ord,Show)

symN1 x = [SymN x]
symT1 x = [SymT x]

makeLenses ''Sym

-- | Define a product to be of vectorial non-terminal to a set of right-hand
-- sides. Each side is a list of terminals and non-terminals. They have the
-- same dimensionality as the left-hand side. In each list "column" can be
-- terminals and non-terminals.
--
-- NOTE We currently do not enforce same dimensionality. Would be nice but
-- TypeLits are not yet ready ...

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

