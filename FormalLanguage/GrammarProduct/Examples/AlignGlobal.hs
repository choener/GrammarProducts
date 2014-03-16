{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The Nussinov RNA secondary structure prediction problem.

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List
import           Data.Vector.Fusion.Util
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Text.Printf

import           ADP.Fusion
import           Data.Array.Repa.Index
import           Data.Array.Repa.Index.Points
import           Data.PrimitiveArray as PA
import           Data.PrimitiveArray.Zero as PA
import           FormalLanguage.CFG

import           FormalLanguage.GrammarProduct



[grammarProduct|
Grammar: Step
N: X
T: c
X -> stp <<< X c
X -> del <<< X
//
Grammar: Stand
N: X
X -> del <<< X
//
Grammar: Done
N: X
T: e
X -> don <<< e
//
Product: Global
Step >< Step  -  Stand * 2  +  Done * 2
//
|]

-- makeAlgebraProductH ['h] ''SigGlobal

score :: Monad m => SigGlobal m Int Int Char () ()
score = SigGlobal
  { delStp = \ x (Z:.():.b ) -> x - 2
  , stpDel = \ x (Z:.a :.()) -> x - 2
  , stpStp = \ x (Z:.a :.b ) -> if a==b then x+1 else -999999
  , donDon = \   (Z:.():.()) -> 0
  , h      = SM.foldl' max 0
  }
{-# INLINE score #-}

forward :: VU.Vector Char -> VU.Vector Char -> ST s (Unboxed (Z:.PointL:.PointL) Int)
forward as bs = do
  let aL = VU.length as
  let bL = VU.length bs
  let aa = chr as
  let bb = chr bs
  !t' <- PA.newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 aL:.pointL 0 bL) (-999999)
  let t = mTblD (Z:.EmptyOk:.EmptyOk) t'
--  fillTable $ gGlobal score t aa bb Empty
  PA.freeze t'
{-# NOINLINE forward #-}

fillTable (MTbl _ t,f) = do
  let (_,Z:.PointL (_:.aL):.PointL (_:.bL)) = boundsM t
  forM_ [0 .. aL] $ \a -> forM_ [0 .. bL] $ \b -> do
    let ix = Z:.pointL 0 a:.pointL 0 b
    (f ix) >>= PA.writeM t ix

main :: IO ()
main = return ()

