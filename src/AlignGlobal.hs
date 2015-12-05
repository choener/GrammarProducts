
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

import           ADP.Fusion
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
Product: Global
Step >< Step  -  Stand * 2  +  Done * 2
//
Emit: Global
|]

makeAlgebraProduct ''SigGlobal



score :: Monad m => SigGlobal m Int Int Char Char
score = SigGlobal
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

pretty :: Monad m => SigGlobal m (String,String) [(String,String)] Char Char
pretty = SigGlobal
  { donDon = \       (Z:.():.()) -> ("","")
  , stpStp = \ (x,y) (Z:.a :.b ) -> (x ++ [a],y ++ [b])
  , delStp = \ (x,y) (Z:.():.b ) -> (x ++ "-",y ++ [b])
  , stpDel = \ (x,y) (Z:.a :.()) -> (x ++ [a],y ++ "-")
  , h      = SM.toList
  }

runNeedlemanWunsch :: Int -> String -> String -> (Int,[(String,String)])
runNeedlemanWunsch k i1' i2' = (d, take k . unId $ axiom b) where
  i1 = VU.fromList i1'
  i2 = VU.fromList i2'
  !(Z:.t) = runNeedlemanWunschForward i1 i2
  d = unId $ axiom t
  !(Z:.b) = gGlobal (score <|| pretty) (toBacktrack t (undefined :: Id a -> Id a)) (chr i1) (chr i2)
{-# NoInline runNeedlemanWunsch #-}

-- | Decoupling the forward phase for CORE observation.

runNeedlemanWunschForward :: Vector Char -> Vector Char -> Z:.(ITbl Id Unboxed (Z:.PointL I:.PointL I) Int)
runNeedlemanWunschForward i1 i2 = let n1 = VU.length i1; n2 = VU.length i2 in mutateTablesDefault $
  gGlobal score
    (ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (PA.fromAssocs (Z:.PointL 0:.PointL 0) (Z:.PointL n1:.PointL n2) (-999999) []))
    (chr i1) (chr i2)
{-# NoInline runNeedlemanWunschForward #-}

main = do
  ls <- lines <$> getContents
  let eats [] = return ()
      eats [x] = return ()
      eats (a:b:xs) = do
        putStrLn a
        putStrLn b
        let (k,ys) = runNeedlemanWunsch 1 a b
        forM_ ys $ \(y1,y2) -> printf "%s %5d\n%s\n" y1 k y2
        eats xs
  eats ls

