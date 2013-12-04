{-# LANGUAGE ParallelListComp #-}

module FormalLanguage.GrammarProduct.Op.Greibach where

import Control.Applicative
import Control.Lens
import Control.Lens.Fold
import Control.Newtype ()
import Data.Function (on)
import Data.List (genericReplicate)
import Data.List (groupBy)
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Semigroup
import qualified Data.Set as S
import Text.Printf

import Text.Trifecta  --
import qualified Data.ByteString.Char8 as B
import           Control.Monad.Trans.State.Strict
import           Data.Default
import           Text.Trifecta.Delta

import FormalLanguage.Grammar
import FormalLanguage.Parser

import FormalLanguage.GrammarProduct.Op.Common



-- * Proof of associativity of the 2-GNF.

-- | Wrap a grammar in 2-GNF form.
--
-- The 2-GNF has rules of the form: X -> a | aY | aYZ with "a" terminal, "Y",
-- "Z" non-terminals.

newtype TwoGNF = TwoGNF {runTwoGNF :: Grammar}

-- | Construct a grammar product for a grammar in 2-GNF form.
--
-- TODO check if grammar is in 2-GNF!

instance Semigroup TwoGNF where
  (TwoGNF g) <> (TwoGNF h) = TwoGNF $ Grammar ts ns es rs s (g^.name ++ h^.name) where
    ts = collectTerminals rs
    ns = collectNonTerminals rs
    es = g^.epsis <> h^.epsis -- this is kind of sketchy
    rs = S.fromList
       . map starRemove
       . catMaybes
       $ [ l <.> r
         | l <- concatMap (starExtend $ gDim g) . S.toList $ g^.rules
         , r <- concatMap (starExtend $ gDim h) . S.toList $ h^.rules
         ]
    s  = liftA2 (\l r -> Symb $ l^.symb ++ r^.symb) (g^.start) (h^.start)
    (<.>) :: Rule -> Rule -> Maybe Rule
    a <.> b | ((Just $ a^.lhs)==g^.start) `exactlyOne` ((Just $ b^.lhs)==h^.start) = Nothing
    a <.> b = Just
            $ Rule (Symb $ a^.lhs.symb ++ b^.lhs.symb)
                   [""]
                   (zipWith (\x y -> Symb $ x^.symb ++ y^.symb) (a^.rhs) (b^.rhs))
    exactlyOne False True  = True
    exactlyOne True  False = True
    exactlyOne _     _     = False
    -- | Extend a rule with ``epsilon-type'' productions to create 2-GNF for all rules
    starExtend :: Int -> Rule -> [Rule]
    starExtend k (Rule l f [t])   = [ Rule l f [t,stars k, stars k]]
    starExtend k (Rule l f [t,n]) = [ Rule l f [t,n,stars k]
                                    , Rule l f [t,stars k,n]
                                    ]
    -- assuming that we have a 2-gnf at most
    starExtend k r                = [r]
    stars :: Int -> Symb
    stars k = Symb $ replicate k E
    -- | Remove star-online columns.
    starRemove :: Rule -> Rule
    starRemove = over rhs (filter (any (not . isEpsilon) . getSymbs))
    isEpsilon E = True
    isEpsilon _ = False

-- | The start symbol for this instance needs to be "Just []" so as to preserve
-- the start symbol in a chain of (<>) operations.

instance Monoid TwoGNF where
  mempty = TwoGNF $ Grammar S.empty S.empty S.empty (S.singleton undefined) (Just $ Symb []) ""
  mappend = (<>)



-- | Takes lists of symbols and aligns according to being
-- terminal/non-terminal:
--
-- aXbc / aXYb =>
--
-- aX-bc    a-Xbc
-- aXYb-    aXYb-
--
-- That is, create all alignments of non-terminals, but just ``left-align'' all
-- terminals. This will create all possible "alignments" of symbols. This is
-- why we return a list of lists.

{-
aligned :: [Symb] -> [Symb] -> [[Symb]]
aligned ls' rs' = go (groupBy ((==) `on` isSymbT) ls') (groupBy ((==) `on` isSymbT) rs') where
  dl = length . getSymbs . head $ ls'
  dr = length . getSymbs . head $ rs'
  go :: [[Symb]] -> [[Symb]] -> [[Symb]]
  go []     []     = []
  go (l:ls) []     = epsR l : go ls []
  go []     (r:rs) = epsL r : go [] rs
  go (l:ls) (r:rs)
    |  all isSymbT l
    && all isSymbT r = goT l r : go ls rs
    |  all isSymbN l
    && all isSymbN r = undefined -- [ ns : gs | ns <- goN l r, gs <- go ls rs ]
    |  all isSymbT l = epsR l : go ls     (r:rs)
    |  all isSymbT r = epsL r : go (l:ls) rs
  goT []     []     = []
  goT ls     []     = epsR ls
  goT []     rs     = epsL rs
  goT (l:ls) (r:rs) = (Symb $ l^.symb ++ r^.symb) : goT ls rs
  goN :: [Symb] -> [Symb] -> [[Symb]]
  goN []     []     = [[]]
  goN (l:ls) []     = epsR [l] : goN ls []
  goN []     (r:rs) = epsL [r] : goN [] rs
  goN lls rrs
    | length lls == length rrs = [[ Symb $ l^.symb ++ r^.symb | l <- lls | r <- rrs ]]
  goN lls@(l:ls) rrs@(r:rs)
    | length lls  < length rrs = undefined
    | length lls  > length rrs = undefined
  epsR ls = map (\(Symb s) -> Symb $ s ++ replicate dr (T "")) ls
  epsL rs = map (\(Symb s) -> Symb $ replicate dl (T "") ++ s) rs
-}

