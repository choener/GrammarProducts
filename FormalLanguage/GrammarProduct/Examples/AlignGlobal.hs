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

makeAlgebraProductH ['h] ''SigGlobal

score :: Monad m => SigGlobal m Int Int Char () ()
score = SigGlobal
  { delStp = \ x (Z:.():.b ) -> x - 2
  , stpDel = \ x (Z:.a :.()) -> x - 2
  , stpStp = \ x (Z:.a :.b ) -> if a==b then x+1 else -999999
  , donDon = \   (Z:.():.()) -> 0
  , h      = SM.foldl' max (-999999)
  }
{-# INLINE score #-}

-- | 
--
-- NOTE The alignment needs to be reversed to print out.

pretty :: Monad m => SigGlobal m [String] (SM.Stream m [String]) Char () ()
pretty = SigGlobal
  { donDon = \       (Z:.():.()) -> [""   ,""   ]
  , stpStp = \ [x,y] (Z:.a :.b ) -> [a  :x,b  :y]
  , delStp = \ [x,y] (Z:.():.b ) -> ['-':x,b  :y]
  , stpDel = \ [x,y] (Z:.a :.()) -> [a  :x,'-':y]
  , h      = return . id
  }

forward :: VU.Vector Char -> VU.Vector Char -> ST s (Unboxed (Z:.PointL:.PointL) Int)
forward as bs = do
  let aL = VU.length as
  let bL = VU.length bs
  let aa = chr as
  let bb = chr bs
  !t' <- PA.newWithM (Z:.pointL 0 0:.pointL 0 0) (Z:.pointL 0 aL:.pointL 0 bL) (-999999)
  let t = mTblD (Z:.EmptyOk:.EmptyOk) t'
  fillTable $ gGlobal score t aa bb Empty Empty
  PA.freeze t'
{-# NOINLINE forward #-}

fillTable (MTbl _ t,f) = do
  let (_,Z:.PointL (_:.aL):.PointL (_:.bL)) = boundsM t
  forM_ [0 .. aL] $ \a -> forM_ [0 .. bL] $ \b -> do
    let ix = Z:.pointL 0 a:.pointL 0 b
    (f ix) >>= PA.writeM t ix

runGlobal :: Int -> String -> String -> (Int,[[String]])
runGlobal k as bs = (t PA.! (Z:.pointL 0 aL:.pointL 0 bL), take k b) where
  aa = VU.fromList as
  bb = VU.fromList bs
  aL = VU.length aa
  bL = VU.length bb
  t = runST $ forward aa bb
  b = backtrack aa bb t
{-# NOINLINE runGlobal #-}

main = do
  ls <- lines <$> getContents
  let eats [] = return ()
      eats [x] = return ()
      eats (a:b:xs) = do
        putStrLn a
        putStrLn b
        let (k,ys) = runGlobal 1 a b
        forM_ ys $ \[y1,y2] -> printf "%s %5d\n%s\n" y1 k y2
        eats xs
  eats ls

backtrack :: VU.Vector Char -> VU.Vector Char -> PA.Unboxed (Z:.PointL:.PointL) Int -> [[String]]
backtrack as bs t' = unId . SM.toList . unId . g $ Z:.pointL 0 aL:.pointL 0 bL where
  aL = VU.length as
  bL = VU.length bs
  aa = chr as
  bb = chr bs
  t = btTblD (Z:.EmptyOk:.EmptyOk) t' g
  (_,g) = gGlobal (score <** pretty) t aa bb Empty Empty
{-# NOINLINE backtrack #-}

