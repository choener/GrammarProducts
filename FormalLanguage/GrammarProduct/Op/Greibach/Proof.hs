{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ParallelListComp #-}

module FormalLanguage.GrammarProduct.Op.Greibach.Proof where

import Control.Lens
import Control.Lens.Fold
import Control.Newtype ()
import Data.List (genericReplicate)
import Data.Monoid hiding ((<>))
import Data.Semigroup
import qualified Data.Set as S
import Text.Printf
import Data.List (groupBy)
import Data.Function (on)
import Data.Maybe
import Control.Applicative

import Text.PrettyPrint.ANSI.Leijen hiding ((<>))
import Text.Trifecta  --
import qualified Data.ByteString.Char8 as B
import           Control.Monad.Trans.State.Strict
import           Data.Default
import           Text.Trifecta.Delta

import FormalLanguage.Grammar
import FormalLanguage.Grammar.PrettyPrint.ANSI
import FormalLanguage.Parser

import FormalLanguage.GrammarProduct.Op.Greibach



-- * Proof of associativity of the 2-GNF.

-- | Run the 2-gnf grammar with the TwoGNF monoid which observes the 2 star
-- cases.

twoGNFassociativity :: (Grammar, Grammar, S.Set Rule, S.Set Rule, Bool)
twoGNFassociativity = ( l
                      , r
                      , (l^.rules) S.\\ (r^.rules)
                      , (r^.rules) S.\\ (l^.rules)
                      , l^.rules == r^.rules)  where
  l = runTwoGNF $ (TwoGNF g <>  TwoGNF g) <> TwoGNF g
  r = runTwoGNF $  TwoGNF g <> (TwoGNF g  <> TwoGNF g)
  g = twoGNFgrammar

twoGNFs = g where
  g = runTwoGNF $ (TwoGNF h <> TwoGNF h)
  h = twoGNFgrammar

assocHelper l r = ( l
                  , r
                  , (l^.rules) S.\\ (r^.rules)
                  , (r^.rules) S.\\ (l^.rules)
                  , l^.rules == r^.rules)

-- * Proof that the 2 star cases are actually needed. We loose associativity
-- without those. As this version does not preserve associativity, we keep it
-- here, instead of the general Greibach version.

newtype FailGNF = FailGNF { runFailGNF :: Grammar }

-- |
--
-- TODO check correctness

instance Semigroup FailGNF where
  (FailGNF g) <> (FailGNF h) = FailGNF $ Grammar ts ns es rs s where
    ts = collectTerminals rs
    ns = collectNonTerminals rs
    es = g^.epsis <> h^.epsis
    rs = S.fromList
       . map starRemove
       . concat
       $ [ l <.> r
         | l <- S.toList $ g^.rules
         , r <- S.toList $ h^.rules
         ]
    s  = liftA2 (\l r -> Symb $ l^.symb ++ r^.symb) (g^.start) (h^.start)
    (<.>) :: Rule -> Rule -> [Rule]
    a <.> b | ((Just $ a^.lhs)==g^.start) `exactlyOne` ((Just $ b^.lhs)==h^.start) = []
    a <.> b
      | [s,m]   <- a^.rhs
      , [t,n,o] <- b^.rhs
      = [ Rule (Symb $ a^.lhs.symb ++ b^.lhs.symb)
          (Fun "")
          [Symb $ s^.symb ++ t^.symb, Symb $ m^.symb ++ n^.symb, Symb $ stars (length $ m^.symb) ^.symb ++ o^.symb ]
        , Rule (Symb $ a^.lhs.symb ++ b^.lhs.symb)
          (Fun "")
          [Symb $ s^.symb ++ t^.symb, Symb $ stars (length $ m^.symb) ^.symb ++ n^.symb, Symb $ m^.symb ++ o^.symb ]
        ]
      | [s,m,o] <- a^.rhs
      , [t,n]   <- b^.rhs
      = [ Rule (Symb $ a^.lhs.symb ++ b^.lhs.symb)
          (Fun "")
          [ Symb $ s^.symb ++ t^.symb
          , Symb $ m^.symb ++ n^.symb
          , Symb $ o^.symb ++ stars (length $ t^.symb) ^.symb
          ]
        , Rule (Symb $ a^.lhs.symb ++ b^.lhs.symb)
          (Fun "")
          [ Symb $ s^.symb ++ t^.symb
          , Symb $ m^.symb ++ stars (length $ t^.symb) ^.symb
          , Symb $ o^.symb ++ n^.symb
          ]
        ]
    a <.> b = [ Rule  (Symb $ a^.lhs.symb ++ b^.lhs.symb)
                      (Fun "")
                      (take 3 $ zipWith (\l r -> Symb $ l^.symb ++ r^.symb) (a^.rhs ++ repeat (stars (gDim g)))
                                                                            (b^.rhs ++ repeat (stars (gDim h)))
                      )
              ]
    exactlyOne False True  = True
    exactlyOne True  False = True
    exactlyOne _     _     = False
    stars :: Int -> Symb
    stars k = Symb . replicate k $ E ""
    -- | Remove star-online columns.
    starRemove :: Rule -> Rule
    starRemove = over rhs (filter (any (not . isEpsilon) . getSymbs))
    isEpsilon (E _) = True
    isEpsilon _     = False


-- | Run the 2-gnf grammar without the star cases.

-- noStarFailure :: (S.Set Rule, S.Set Rule, 
noStarFailure = assocHelper l r where
  l = runFailGNF $ (FailGNF g <>  FailGNF g) <> FailGNF g
  r = runFailGNF $  FailGNF g <> (FailGNF g  <> FailGNF g)
  g = twoGNFgrammar

-- * The simple 2-gnf grammar to run the proof on.

-- | Very simple 2-gnf form for proofs.

twoGNFgrammar = case g of
  Success g' -> g'
  Failure f  -> error $ show f
  where
  g = parseString
        ((evalStateT . runGrammarP) grammar def)
        (Directed (B.pack "testGrammar") 0 0 0 0)
        twoGNF
  twoGNF = unlines
    [ "Grammar: TwoGNF"
    , "N: A"
    , "N: B"
    , "N: C"
    , "N: D"
    , "T: a"
    , "T: b"
    , "T: c"
--    , "S: X"
    , "A -> three <<< a B C"
    , "A -> two   <<< b D"
    , "A -> one   <<< c"
    , "//"
    ]

