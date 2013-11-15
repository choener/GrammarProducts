
module FormalLanguage.GrammarProduct.Op.Greibach where

import Control.Lens
import Control.Lens.Fold
import Control.Newtype
import Data.List (genericReplicate)
import Data.Monoid hiding ((<>))
import Data.Semigroup
import qualified Data.Set as S
import Text.Printf
import Data.List (groupBy)
import Data.Function (on)

import FormalLanguage.Grammar



-- | Wrap a grammar in 2-GNF form.
--
-- The 2-GNF has rules of the form: X -> a | aY | aYZ with "a" terminal, "Y",
-- "Z" non-terminals.

newtype TwoGNF = TwoGNF {runTwoGNF :: Grammar}

-- | Construct a grammar product for a grammar in 2-GNF form.
--
-- TODO check if grammar is in 2-GNF!

instance Semigroup TwoGNF where
  (TwoGNF g) <> (TwoGNF h) = TwoGNF $ Grammar ts ns rs s where
    ts = undefined
    ns = undefined
    rs = S.fromList . concat $ [ l <.> r | l <- S.toList (g^.rules), r <- S.toList (h^.rules) ]
    s  = Symb $ g^.start.symb ++ h^.start.symb
    (<.>) :: Rule -> Rule -> [Rule]
    a <.> b | (a^.lhs==g^.start) `exactlyOne` (b^.lhs==h^.start) = []
    (Rule al af ars) <.> (Rule bl bf brs) = map (Rule (Symb $ al^.symb ++ bl^.symb) undefined) (aligned ars brs)
    {-
    -- X -> a <.> X -> a
    go2gnf [a] [b] = [[Symb $ a^.symb ++ b^.symb]]
    -- X -> a <.> X -> aY
    go2gnf [a] [b1,b2]    = [[Symb $ a^.symb ++ b1^.symb], [Symb $ undefined ++ b2^.symb]] -- TODO epsilon of size b1
    -- X -> a <.> X -> aYZ
    go2gnf [a] [b1,b2,b3] = [[Symb $ a^.symb ++ b1^.symb], [Symb $ undefined ++ b2^.symb], [Symb $ undefined ++ b3^.symb]]
    -- X -> aY <.> X -> a
    go2gnf [a1,a2] [b1] = [[Symb $ a1^.symb ++ b1^.symb], [Symb $ a2^.symb ++ undefined]]
    -- X -> aY <.> X -> aY
    go2gnf [a1,a2] [b1,b2] = [[Symb $ a1^.symb ++ b1^.symb], [Symb $ a2^.symb ++ b2^.symb]]
    -- X -> aY <.> X -> aYZ
    go2gnf [a1,a2] [b1,b2,b3]
    -- X -> aYZ <.> X -> a
    -- X -> aYZ <.> X -> aY
    -- X -> aYZ <.> X -> aYZ
    go2gnf ars brs = error $ "cannot handle: " ++ show (ars,brs)
    -}
    exactlyOne False True  = True
    exactlyOne True  False = True
    exactlyOne _     _     = False

instance Monoid TwoGNF where
  mempty = TwoGNF $ Grammar S.empty S.empty (S.singleton undefined) (Symb [])
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

aligned :: [Symb] -> [Symb] -> [[Symb]]
aligned ls' rs' = go (groupBy ((==) `on` tSymb) ls') (groupBy ((==) `on` tSymb) rs') where
  dl = length . getSymbs . head $ ls'
  dr = length . getSymbs . head $ rs'
  go :: [[Symb]] -> [[Symb]] -> [[Symb]]
  go []     []     = []
  go (l:ls) []     = epsR l : go ls []
  go []     (r:rs) = epsL r : go [] rs
  go (l:ls) (r:rs)
    |  all tSymb l
    && all tSymb r  = goT l r : go ls rs
    |  all nSymbG l
    && all nSymbG r = [ ns : gs | ns <- goN l r, gs <- go ls rs ]
    |  all tSymb l  = epsR l : go ls     (r:rs)
    |  all tSymb r  = epsL r : go (l:ls) rs
  goT []     []     = []
  goT ls     []     = epsR ls
  goT []     rs     = epsL rs
  goT (l:ls) (r:rs) = (Symb $ l^.symb ++ r^.symb) : goT ls rs
  goN = undefined
  epsR ls = map (\(Symb s) -> Symb $ s ++ replicate dr (T "")) ls
  epsL rs = map (\(Symb s) -> Symb $ replicate dl (T "") ++ s) rs



{-
instance Semigroup (Direct Grammar) where
  (Direct l) <> (Direct r) = Direct $ Grammar xs (l^.gname ++ "," ++ r^.gname) where
    xs = S.fromList [ direct pl pr | pl <- S.toList (l^.ps), pr <- S.toList (r^.ps) ]
    direct (PR [Nt dl sl] ls fl) (PR [Nt dr sr] rs fr)
      | (length $ filter isNt ls) > 1 || (length $ filter isNt rs) > 1
      = error "direct products are implemented only for linear grammars as of now"
      | otherwise
      = PR [Nt (dl+dr) (sl++sr)] zs (fl ++ fr)
      where
        dls = map (^.dim) ls
        drs = map (^.dim) rs
        zs  = mergeRHS ls rs

-- TODO check that 'mempty' does not explode somewhere ;-) == the neutral
-- element is weird in code, too

instance Monoid (Direct Grammar) where
  mempty = Direct $ Grammar r "" where
    r = S.singleton $ PR [] [] []
  mappend = (<>)

-- | Merges right-hand sides in a direct product. Currently only defined for
-- linear grammars (and certain types of CFGs). Basically, two or more
-- non-terminals next to each other will fail.

mergeRHS :: [NtT] -> [NtT] -> [NtT]
mergeRHS [] rs = rs -- neutral element
mergeRHS ls [] = ls -- neutral element
mergeRHS ls' rs' = concat $ go (groupRHS ls') (groupRHS rs') where
  dl = head $ map (^.dim) ls'
  dr = head $ map (^.dim) rs'
  go [] [] = []
  go [] (r:rs)
    | all isT  r = (map (\(T d ts) -> T (dl+d) (genericReplicate dl (TSym "")++ts)) r) : go [] rs
    | all isNt r = let [Nt d nts] = r
                   in  [Nt (dl+d) (genericReplicate dl epsilonNtSym ++ nts)] : go [] rs
  go (l:ls) []
    | all isT  l = (map (\(T d ts) -> T (d+dr) (ts++genericReplicate dr (TSym ""))) l) : go ls []
    | all isNt l = let [Nt d nts] = l
                   in  [Nt (d+dr) (nts ++ genericReplicate dr epsilonNtSym)] : go ls []
  go (l:ls) (r:rs)
    | all isT  l && all isT  r = goT l r : go ls rs
    | all isNt l && all isNt r =
        let [Nt dll nls] = l
            [Nt drr nrs] = r
        in  [Nt (dll+drr) (nls++nrs)] : go ls rs
    | all isT l   = go [l] [] ++ go ls (r:rs)
    | all isT r   = go [] [r] ++ go (l:ls) rs
    | otherwise   = go [l] [] ++ go [] [r] ++ go ls rs
  goT [] [] = []
  goT [] (T d ts :rs) = T (dl+d) (genericReplicate dl epsilonTSym ++ ts) : goT [] rs
  goT (T d ts :ls) [] = T (d+dr) (ts ++ genericReplicate dr epsilonTSym) : goT ls []
  goT (T dll tsl :ls) (T drr tsr :rs) = T (dll+drr) (tsl++tsr) : goT ls rs
-}

