{-# LANGUAGE PatternGuards #-}

module FormalLanguage.GrammarProduct.Op.Chomsky where

import Control.Applicative
import Control.Lens
import Control.Lens.Fold
import Control.Newtype ()
import Data.Function (on)
import Data.List (genericReplicate,replicate,groupBy)
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Semigroup
import qualified Data.Set as S
import Text.Printf

import FormalLanguage.Grammar
import FormalLanguage.Parser
import FormalLanguage.GrammarProduct.Op.Common



newtype CNF = CNF { runCNF :: Grammar }

instance Semigroup CNF where
  (CNF g) <> (CNF h) = CNF $ Grammar ts ns es rs s (g^.name ++ h^.name) where
    ts = S.fromList $ g^..tsyms.folded ++ h^..tsyms.folded
    ns = collectNonTerminals rs -- this is needed since we generate completely new non-terminal symbols
    es = S.fromList $ g^..epsis.folded ++ h^..epsis.folded
    rs = S.fromList
       . concat
       $ [ chomskyCombine l r | l <- g^..rules.folded, r <- h^..rules.folded ]
    s  = liftA2 (\l r -> Symb $ l^.symb ++ r^.symb) (g^.start) (h^.start)

instance Monoid CNF where
  mempty = CNF $ Grammar S.empty S.empty S.empty (S.singleton undefined) (Just $ Symb []) ""
  mappend = (<>)

-- | Combine production rules a la Chomsky normal form.
--
-- TODO We need to be able to generate fresh rule name, as we are splitting
-- rules here! (this means that we need to lift this stuff into a
-- name-generating monad)

chomskyCombine :: Rule -> Rule -> [Rule]
chomskyCombine (Rule l f rs) (Rule a g bs)
  | [r] <- rs, [b] <- bs, tSymb r, tSymb b
  = [Rule (Symb $ l^.symb ++ a^.symb) (f++g) [Symb $ r^.symb ++ b^.symb]]
  | [r1,r2] <- rs, [b1,b2] <- bs, nSymb r1, nSymb r2, nSymb b1, nSymb b2
  = [Rule (Symb $ l^.symb ++ a^.symb) (f++g) [Symb $ r1^.symb ++ b1^.symb, Symb $ r2^.symb ++ b2^.symb]]
  | [r] <- rs, [b1,b2] <- bs, tSymb r, nSymb b1, nSymb b2
  = {- concatMap extendRederive -}
      [Rule (Symb $ l^.symb ++ a^.symb) (f++g) [Symb $ r^.symb  ++ b1^.symb, Symb $ genEps r ++ b2^.symb]
      ,Rule (Symb $ l^.symb ++ a^.symb) (f++g) [Symb $ genEps r ++ b1^.symb, Symb $ r^.symb  ++ b2^.symb]
      ]
  | [r1,r2] <- rs, [b] <- bs, nSymb r1, nSymb r2, tSymb b
  = {- concatMap extendRederive -}
      [Rule (Symb $ l^.symb ++ a^.symb) (f++g) [Symb $ r1^.symb ++ b^.symb , Symb $ r2^.symb ++ genEps b]
      ,Rule (Symb $ l^.symb ++ a^.symb) (f++g) [Symb $ r1^.symb ++ genEps b, Symb $ r2^.symb ++ b^.symb ]
      ]
  | otherwise = error $ "cannot handle (rule is not CNF): " ++ show (Rule l f rs, Rule a g bs)

-- | Extend mixed rules and rederive CNF

{-
extendRederive :: Rule -> [Rule]
extendRederive (Rule l f [r]) | tSymb r = [Rule l f [r]]
extendRederive (Rule l f [r1,r2])
  | not (tSymb r1) && not (nSymb r1) && nSymb r2
  = [Rule (Symb $ l^.symb ++ a^.
extendRederive r = error $ "cannot handle (rule not in extendRederive form for CNF): " ++ show r
-}

-- | Generate a certain number of epsilons

genEps :: Symb -> [TN]
genEps s = replicate (length $ s^.symb) eps

