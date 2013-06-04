{-# LANGUAGE FlexibleInstances #-}

module BioInf.GrammarProducts.Op.Add where

import Control.Newtype
import Data.Semigroup
import Control.Lens
import Control.Lens.Fold
import qualified Data.Set as S
import Data.List (genericReplicate)
import Text.Printf
import Data.Monoid hiding ((<>))

import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.Helper



-- | Add two grammars. Implemented as the union of production rules without any
-- renaming.

newtype Add a = Add {unAdd :: a}



instance Semigroup (Add Grammar) where
  (Add l) <> (Add r)
    | grammarDim l /= grammarDim r = error $ printf "ERROR: grammars \n%s\n and \n%s\n have different dimensions, cannot unify." (show l) (show r)
    | otherwise = Add $ Grammar (S.union (l^.ps) (r^.ps)) (l^.gname ++ "," ++ r^.gname)

instance Monoid (Add Grammar) where
  mempty = Add $ Grammar (S.empty) ""
  mappend = (<>)

-- idempotency is not made explicit here

