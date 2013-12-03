{-# LANGUAGE FlexibleInstances #-}

module FormalLanguage.GrammarProduct.Op.Subtract where

import Control.Newtype
import Data.Semigroup
import Control.Lens
import Control.Lens.Fold
import qualified Data.Set as S
import Data.List (genericReplicate)
import Text.Printf

import FormalLanguage.Grammar

import FormalLanguage.GrammarProduct.Op.Common



-- | Subtract two grammars.

subtract :: Grammar -> Grammar -> Grammar
subtract l r
    | gDim l /= gDim r = error $ printf "grammars %s and %s have different dimensions, cannot unify. (subtract)" (show l) (show r)
    | otherwise        = Grammar ts ns es rs s (l^.name ++ r^.name) where
        ts = collectTerminals rs
        ns = collectNonTerminals rs
        es = collectEpsilons rs
        rs = (l^.rules) S.\\ (r^.rules)
        s  = case (l^.start) of
               Nothing -> Nothing
               Just s' -> if anyOf (rules.folded.lhs) (==s') l
                            then l^.start
                            else Nothing

