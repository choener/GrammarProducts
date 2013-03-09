{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
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
{-# LANGUAGE ViewPatterns #-}

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

import Control.Monad (liftM)
import Data.ByteString.Char8 (pack)
import Data.Semigroup
import Data.Set (Set(..))
import Data.Vector (Vector (..))
import GHC.TypeLits
import qualified Data.Foldable as F
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Function
import Data.List

import BioInf.GrammarProducts.Parser
import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.TH



qqGDhere = qqGD $ GrammarOperations
  { productOperations = [("*", grammarProduct)]
  }

-- * once more, with feeling

-- | The "normal" product operation
--
-- TODO need better "comb" operation:
-- - we want to "group" on terminal / non-terminal
-- - introduce "-" terminals where required in the terminal groups
-- - produce an error if non-terminal groups are of uneven size ?!

grammarProduct :: Grammar -> Grammar -> Grammar
grammarProduct x y = Grammar
  { name = name x ++ "-" ++ name y
  , terminals = [ VSym $ a++b | VSym a <- terminals x, VSym b <- terminals y ]
  , nonterms  = [ VSym $ a++b | VSym a <- terminals x, VSym b <- terminals y ]
  , functions = [ fx ++ fy | fx <- functions x, fy <- functions y ]
  , rules     = [ Rule (VSym $ a++b) (f++"_"++g) (ruleProduct as bs)
                | Rule (VSym a) f as <- rules x
                , Rule (VSym b) g bs <- rules y ]
  }

-- |

ruleProduct :: [VSym] -> [VSym] -> [VSym]
ruleProduct [] ys = error $ "ruleProduct applied to empty production: " ++ show ("empty",ys)
ruleProduct xs [] = error $ "ruleProduct applied to empty production: " ++ show (xs,"empty")
ruleProduct xs' ys' = combine xs ys where
  xs = groupBy ((==) `on` symType) xs'
  ys = groupBy ((==) `on` symType) ys'
  combine :: [[VSym]] -> [[VSym]] -> [VSym]
  combine [] [] = []
  combine (as:ass) []
    | symType as == N = error $ "can't handle unmatched non-terminals in: " ++ show (xs',ys')
    | otherwise       = prod as [] ++ combine ass []
  combine [] (bs:bss)
    | symType bs == N = error $ "can't handle unmatched non-terminals in: " ++ show (xs',ys')
    | otherwise       = prod [] bs ++ combine [] bss
  combine (as:ass) (bs:bss)
    | symType as == symType bs = prod as bs ++ combine ass bss
    | symType as == T          = prod as [] ++ combine ass (bs:bss)
    | symType bs == T          = prod [] bs ++ combine (as:ass) bss
  prod :: [VSym] -> [VSym] -> [VSym]
  -- lonely terminal in as
  prod as []
    | symType as == T = take (length as) $ zipWith (\(VSym a) (VSym b) -> VSym $ a++b) as bs'
  -- lonely terminal in bs
  prod [] bs
    | symType bs == T = take (length bs) $ zipWith (\(VSym a) (VSym b) -> VSym $ a++b) as' bs
  -- combination of non-terminals
  prod as bs
    | symType as == N
    , symType bs == N
    , length as == length bs = zipWith (\(VSym a) (VSym b) -> VSym $ a++b) as bs
    -- non-terminal special error handling
    | symType as == N
    , symType bs == N = error $ "can't handle different-length group of non-terminals: " ++ show (xs',ys')
    -- terminals of different lengths are handled by injecting ``delete'' symbols
    | symType as == T
    , symType bs == T = take (max (length as) (length bs)) $ zipWith (\(VSym a) (VSym b) -> VSym $ a++b) (as++as') (bs++bs')
  -- some other error occured
  prod _ _ = error $ "ran into a 'prod' error: " ++ show (xs',ys')
  as' = let (VSym a:_) = xs' in repeat (VSym $ replicate (length a) (Sym T "-"))
  bs' = let (VSym b:_) = ys' in repeat (VSym $ replicate (length b) (Sym T "-"))

-- | Removal of a symbol and cleanup of rules

-- | consistency check





-- ** old



--testF (f :: forall c . Show c => c -> String) (a,b) = (f a, f b)

