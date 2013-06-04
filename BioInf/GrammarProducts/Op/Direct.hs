{-# LANGUAGE FlexibleInstances #-}

module BioInf.GrammarProducts.Op.Direct where

import Control.Newtype
import Data.Semigroup
import Control.Lens
import qualified Data.Set as S
import Data.List (genericReplicate)
import Data.Monoid hiding ((<>))

import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.Helper



-- | Direct product of two grammars.
--
-- Currently implemented for linear grammars. Once we move to context-free
-- grammars with more than one non-terminal on the RHS, things become
-- interesting. Consult:
--
-- Hoener zu Siederdissen, Hofacker, Stadler, 2013
-- /Grammar Products Paper/

newtype Direct a = Direct {unDirect :: a}



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

