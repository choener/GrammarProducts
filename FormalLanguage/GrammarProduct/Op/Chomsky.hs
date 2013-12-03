{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE LambdaCase #-}
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
import System.IO.Unsafe

import FormalLanguage.Grammar
import FormalLanguage.Parser
import FormalLanguage.Grammar.PrettyPrint.ANSI

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
  = [Rule (Symb $ l^.symb ++ a^.symb) [] {- (f++g) -} [Symb $ r^.symb ++ b^.symb]]
  | [r1,r2] <- rs, [b1,b2] <- bs, nSymb r1, nSymb r2, nSymb b1, nSymb b2
  = [Rule (Symb $ l^.symb ++ a^.symb) [] {- (f++g) -} [Symb $ r1^.symb ++ b1^.symb, Symb $ r2^.symb ++ b2^.symb]]
  | [r] <- rs, [b1,b2] <- bs, tSymb r, nSymb b1, nSymb b2
  = let (z1,zs1) = symbToRules r b1
        (z2,zs2) = symbToRules r b2
    in  zs1 ++ zs2 ++ {-concatMap (extendRederive (length $ l^.symb) (length $ a^.symb))-}
        [ Rule (Symb $ l^.symb ++ a^.symb) [] {- (f++g) -} [ {- Symb $ r^.symb  ++ b1^.symb -} z1 , Symb $ genEps r ++ b2^.symb]
        , Rule (Symb $ l^.symb ++ a^.symb) [] {- (f++g) -} [Symb $ genEps r ++ b1^.symb, z2 {- Symb $ r^.symb  ++ b2^.symb -} ]
        ]
  | [r1,r2] <- rs, [b] <- bs, nSymb r1, nSymb r2, tSymb b
  = let (z1,zs1) = symbToRules r1 b
        (z2,zs2) = symbToRules r2 b
    in  zs1 ++ zs2 ++ {-concatMap (extendRederive (length $ l^.symb) (length $ a^.symb))-}
        [ Rule (Symb $ l^.symb ++ a^.symb) [] {- (f++g) -} [{- Symb $ r1^.symb ++ b^.symb -} z1 , Symb $ r2^.symb ++ genEps b]
        , Rule (Symb $ l^.symb ++ a^.symb) [] {- (f++g) -} [Symb $ r1^.symb ++ genEps b, z2 {- Symb $ r2^.symb ++ b^.symb -} ]
        ]
  --
  -- extended Chomsky: Non-terminal -> Non-terminal
  --
  {-
  | [r] <- rs, [b] <- bs, nSymb r, nSymb b
  = [ Rule (Symb $ l^.symb ++ a^.symb) [] [ Symb $ r^.symb ++ b^.symb ] ]
  | [r] <- rs, [b1,b2] <- bs, nSymb r, nSymb b1, nSymb b2
  = []
  | [r1,r2] <- rs, [b] <- bs, nSymb r1, nSymb r2, nSymb b
  = []
  | [r] <- rs, [b] <- bs
  = []
  -}
  {-
  = [ Rule (Symb $ l^.symb ++ a^.symb) [] [ Symb $ r^.symb  ++ b1^.symb, Symb $ genEps r ++ b2^.symb ]
    , Rule (Symb $ l^.symb ++ a^.symb) [] [ Symb $ genEps r ++ b1^.symb, Symb $ r^.symb  ++ b2^.symb ]
    ]
  -}
  --
  -- extended Chomsky above
  --
  | otherwise = unsafePerformIO $ do
      print "======"
      printDoc $ rulesDoc $ S.singleton $ Rule l f rs
      printDoc $ rulesDoc $ S.singleton $ Rule a g bs
      fail "cannot handle (rule is not CNF):"
  -- | otherwise = error $ "cannot handle (rule is not CNF): " ++ show (printDoc $ rulesDoc $ S.singleton $ Rule l f rs, Rule a g bs)

{-
-- | Extend mixed rules and rederive CNF

extendRederive :: Int -> Int -> Rule -> [Rule]
extendRederive α β (Rule l f [r1,r2])
  | not (tSymb r1) && not (nSymb r1) && nSymb r2
  = let (newN,epsN,trmN,epsT) = genNewSymbols α β r1
    in  [ Rule l    f           [newN,r2]
        , Rule newN ( {- "nwNL_": -} f) [epsN,trmN]
        , Rule newN ( {- "nwNR_": -} f) [trmN,epsN]
        , Rule trmN ( {- "trmN_": -} f) [epsT]
        ]
  | nSymb r1 && not (tSymb r2) && not (nSymb r2)
  = let (newN,epsN,trmN,epsT) = genNewSymbols α β r2
    in  [ Rule l    f           [r1,newN]
        , Rule newN ( {- "nwNL_": -} f) [epsN,trmN]
        , Rule newN ( {- "nwNR_": -} f) [trmN,epsN]
        , Rule trmN ( {- "trmN_": -} f) [epsT]
        ]
extendRederive _ _ r = error $ "cannot handle (rule not in extendRederive form for CNF): " ++ show r

genNewSymbols :: Int -> Int -> Symb -> (Symb,Symb,Symb,Symb)
genNewSymbols α β x = (newN, epsN, trmN, epsT) where
  -- the new non-terminal, with term TN's replaced by non-term TN with same name (plus extension)
  newN = Symb . map (\case (T s) -> N ("N"++s) Singular ; z -> z) $ x^.symb
  -- the new non-terminal, with terms replaced by epsilons
  epsN = Symb . map (\case (T s) -> eps                 ; z -> z) $ x^.symb
  -- the new non-terminal for the terminal symbol, with terms replaced by non-term symbols
  -- TODO we can't just replace all N here with eps, tome could have been created from other prods.
  trmN = Symb . map (\case (T s) -> N ("T"++s) Singular ; N _ _ -> eps; z -> z) $ x^.symb
  -- finally the terminal 
  epsT = Symb . map (\case (N _ _) -> eps               ; z -> z) $ x^.symb
-}

-- | 

symbToRules :: Symb -> Symb -> (Symb, [Rule])
symbToRules u' l'
  | nSymb u' && tSymb l' = go u' l'
  | tSymb u' && nSymb l' = let (s,rs) = go (over symb reverse l') (over symb reverse u')
                           in  ( over symb reverse s
                               , map (\(Rule l [] rs) -> Rule (over symb reverse l) [] (map (over symb reverse) rs)) rs
                               )
  | otherwise            = error $ "incompatible upper/lower: " ++ show (u',l')
  where
    -- in 'n' we have the partial non-terminal, in 't' the partial terminal
    go n t =
      let t' = Symb $ map (\case (T s) -> (N ("T"++s) Singular) ; z -> z) $ t^.symb
      in  ( Symb $ n^.symb ++ t'^.symb
          , [ Rule (Symb $ n^.symb ++ t'^.symb) [] [ Symb $ n^.symb ++ genEps t, Symb $ genEps n ++ genTermStar t ]
            , Rule (Symb $ n^.symb ++ t'^.symb) [] [ Symb $ genEps n ++ genTermStar t, Symb $ n^.symb ++ genEps t ]
            , Rule (Symb $ genEps n ++ genTermStar t)  [] [ Symb $ genEps n ++ t^.symb ]
            ]
          )

-- | Generate a certain number of epsilons

genTermStar :: Symb -> [TN]
genTermStar s = map (\case (T s) -> N ("S"++s) Singular ; z -> z) $ s^.symb

