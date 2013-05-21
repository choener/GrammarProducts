{-# LANGUAGE FlexibleInstances #-}

module BioInf.GrammarProducts.Op.Subtract where

import Control.Newtype
import Data.Semigroup
import Control.Lens
import Control.Lens.Fold
import qualified Data.Set as S
import Data.List (genericReplicate)
import Text.Printf

import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.Helper



-- | Subtract two grammars.


subtract :: Grammar -> Grammar -> Grammar
subtract l r
    | grammarDim l /= grammarDim r = error $ printf "grammars %s and %s have different dimensions, cannot unify." (show l) (show r)
    | otherwise = Grammar xs (l^.gname ++ "," ++ r^.gname)
    where
      xs = (l^.ps) S.\\ (r^.ps)

