{-# LANGUAGE FlexibleInstances #-}

module FormalLanguage.GrammarProduct.Op.Subtract where

import           Control.Arrow ((&&&))
import           Control.Lens.Fold
import           Control.Lens hiding (outside)
import           Control.Newtype
import           Data.List (genericReplicate)
import           Data.Semigroup
import qualified Data.Map as M
import qualified Data.Set as S
import           Text.Printf

import           FormalLanguage.CFG.Grammar

import           FormalLanguage.GrammarProduct.Op.Common



-- | Subtract two grammars.

subtract :: Grammar -> Grammar -> Grammar
subtract l r
    | dim l /= dim r           = error $ printf "grammars %s and %s have different dimensions, cannot unify. (subtract)" (show l) (show r)
    | l^.outside /= r^.outside = error $ printf "grammars %s and %s have different inside/outside annotation." (show l) (show r)
    | otherwise                = g
    where sv = M.fromList . map ((_name &&& id) . fst) . uniqueSynVarsWithTape       $ g
          st = M.fromList . map ((_name &&& id) . fst) . uniqueSynTermsWithTape      $ g
          tv = M.fromList . map ((_name &&& id) . fst) . uniqueBindableTermsWithTape $ g
          io = l^.outside
          rs = (l^.rules) S.\\ (r^.rules)
          s  = if (anyOf (folded . lhs) ((l^.start) ==) rs) then l^.start else Symbol []
          p  = M.union (l^.params) (r^.params)
          g  = Grammar sv st tv io rs s p (l^.grammarName ++ r^.grammarName) False

{-
subtract l r
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
-}

