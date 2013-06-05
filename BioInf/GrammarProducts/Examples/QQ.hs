{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module BioInf.GrammarProducts.Examples.QQ where

import qualified Language.Haskell.TH as TH
import Data.Array.Repa.Index
import Data.Array.Repa.Shape
import Data.Vector.Fusion.Stream.Monadic (Stream)
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Fusion.Stream as S

import ADP.Fusion
import ADP.Fusion.Empty
import ADP.Fusion.Multi

import BioInf.GrammarProducts.TH


[qqGDG|
Grammar: Test
NT: X
T:  a
T:  empty
X -> f <<< X a
//
Grammar: Stop
NT: X
T:  empty
X -> nil <<< empty
//
Product: Prod
Test >< Test + Stop >< Stop
//
|]

step :: Monad m => SProd m Int Int Char ()
step = SProd
  { f_f = \i (Z:.a:.b) -> if a==b then i+1 else i
  , nil_nil = \(Z:.():.()) -> 0
  , h = SM.foldl' max 0
  }
{-# INLINE step #-}

