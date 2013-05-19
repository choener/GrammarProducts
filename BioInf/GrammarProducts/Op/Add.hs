{-# LANGUAGE FlexibleInstances #-}

module BioInf.GrammarProducts.Op.Add where

import Control.Newtype
import Data.Semigroup
import Control.Lens
import Control.Lens.Fold
import qualified Data.Set as S
import Data.List (genericReplicate)
import Text.Printf

import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.Helper



-- | Add two grammars. Implemented as the union of production rules without any
-- renaming.

newtype Add a = Add {unAdd :: a}



instance Semigroup (Add Grammar) where
  (Add l) <> (Add r)
    | dl /= dr  = error $ printf "grammars %s and %s have different dimensions, cannot unify."
    | otherwise = Add $ Grammar xs (l^.gname ++ "," ++ r^.gname)
    where
      dl = gD $ l^.ps
      dr = gD $ r^.ps
      gD = head . map (^.lhs.to head.dim) . S.toList
      xs = S.union (l^.ps) (r^.ps)

