
-- | The Nussinov RNA secondary structure prediction problem.

module Main where

import           Control.Applicative ()
import           Control.Monad
import           Control.Monad.ST
import           Data.Char (toUpper,toLower)
import           Data.List
import           Data.Vector.Fusion.Util
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import qualified Data.Vector.Fusion.Stream.Monadic as SM
import qualified Data.Vector.Unboxed as VU
import           Data.Vector.Unboxed (Vector)
import           Text.Printf
import           Data.Sequence ((|>),Seq,empty)
import           Data.Foldable (toList)

import           ADPfusion.PointL
import           Data.PrimitiveArray as PA hiding (map,toList)
import           FormalLanguage.CFG

import           FormalLanguage.GrammarProduct



[grammarProduct|
Grammar: Step
N: X
T: c
S: X
X -> stp <<< X c
X -> del <<< X
//
Grammar: Stand
N: X
S: X
X -> del <<< X
//
Grammar: Done
N: X
S: X
X -> don <<< e
//
Product: Glbl
Step >< Step  -  Stand * 2  +  Done * 2
//
Emit: Glbl
|]

makeAlgebraProduct ''SigGlbl



score :: Monad m => SigGlbl m Int Int Char Char
score = SigGlbl
  { donDon = \   (Z:.():.()) -> 0
  , stpStp = \ x (Z:.a :.b ) -> if a==b then x+1 else -999999
  , delStp = \ x (Z:.():.b ) -> x - 2
  , stpDel = \ x (Z:.a :.()) -> x - 2
  , h       = SM.foldl' max (-999999)
  }
{-# INLINE score #-}

-- |
--
-- TODO use fmlist to make this more efficient.

pretty :: Monad m => SigGlbl m (String,String) [(String,String)] Char Char
pretty = SigGlbl
  { donDon = \       (Z:.():.()) -> ("","")
  , stpStp = \ (x,y) (Z:.a :.b ) -> (x ++ [a],y ++ [b])
  , delStp = \ (x,y) (Z:.():.b ) -> (x ++ "-",y ++ [b])
  , stpDel = \ (x,y) (Z:.a :.()) -> (x ++ [a],y ++ "-")
  , h      = SM.toList
  }

runNeedlemanWunsch :: Int -> String -> String -> (Int,[(String,String)],String)
runNeedlemanWunsch k i1' i2' = (d, take k . unId $ axiom b, show perf) where
  i1 = VU.fromList i1'
  i2 = VU.fromList i2'
  Mutated (Z:.t) perf eachPerf = runNeedlemanWunschForward i1 i2
  d = unId $ axiom t
  !(Z:.b) = gGlbl (score <|| pretty) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline runNeedlemanWunsch #-}

-- | Decoupling the forward phase for CORE observation.

runNeedlemanWunschForward
  :: Vector Char
  -> Vector Char
  -> Mutated (Z:.TwITbl 0 0 Id (Dense VU.Vector) (Z:.EmptyOk:.EmptyOk) (Z:.PointL I:.PointL I) Int)
{-# NoInline runNeedlemanWunschForward #-}
runNeedlemanWunschForward i1 i2 = runST $ do
  arr <- newWithPA (ZZ:..LtPointL n1:..LtPointL n2) (-999999)
  ts <- fillTables $ gGlbl score
          (ITbl @_ @_ @_ @_ @0 @0 (Z:.EmptyOk:.EmptyOk) arr)
          (chr i1) (chr i2)
  return ts
  where !n1 = VU.length i1
        !n2 = VU.length i2
  {-
    -}

main = do
  ls <- lines <$> getContents
  let eats [] = return ()
      eats [x] = return ()
      eats (a:b:xs) = do
        putStrLn a
        putStrLn b
        let (k,ys,p) = runNeedlemanWunsch 1 a b
        forM_ ys $ \(y1,y2) -> printf "%s %5d\n%s\n" y1 k y2
        putStrLn p
        eats xs
  eats ls

