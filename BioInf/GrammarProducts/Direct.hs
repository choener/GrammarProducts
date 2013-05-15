{-# LANGUAGE FlexibleInstances #-}

module BioInf.GrammarProducts.Direct where

import Control.Newtype
import Data.Semigroup
import Control.Lens
import qualified Data.Set as S
import Data.List (genericReplicate)

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
    direct (PR [Nt dl sl gl] ls) (PR [Nt dr sr gr] rs)
      | (length $ filter isNt ls) > 1 || (length $ filter isNt rs) > 1
      = error "direct products are implemented only for linear grammars as of now"
      | otherwise
      = PR [Nt (dl+dr) (sl++sr) (-1)] zs
      where
        dls = map (^.dim) ls
        drs = map (^.dim) rs
        zs  = mergeRHS ls rs

-- | Merges right-hand sides in a direct product. Currently only defined for
-- linear grammars (and certain types of CFGs). Basically, two or more
-- non-terminals next to each other will fail.

mergeRHS :: [NtT] -> [NtT] -> [NtT]
mergeRHS ls' rs' = concat $ go (groupRHS ls') (groupRHS rs') where
  dl = head $ map (^.dim) ls'
  dr = head $ map (^.dim) rs'
  go [] [] = []
  go [] (r:rs)
    | all isT  r = (map (\(T d ts _) -> T (dl+d) (genericReplicate dl (TSym "")++ts) 0) r) : go [] rs
    | all isNt r = let [Nt d nts _] = r
                   in  [Nt (dl+d) (genericReplicate dl epsilonNtSym ++ nts) 0] : go [] rs
  go (l:ls) []
    | all isT  l = (map (\(T d ts _) -> T (d+dr) (ts++genericReplicate dr (TSym "")) 0) l) : go ls []
    | all isNt l = let [Nt d nts _] = l
                   in  [Nt (d+dr) (nts ++ genericReplicate dr epsilonNtSym) 0] : go ls []
  go (l:ls) (r:rs)
    | all isT  l && all isT  r = goT l r : go ls rs
    | all isNt l && all isNt r =
        let [Nt dll nls _] = l
            [Nt drr nrs _] = r
        in  [Nt (dll+drr) (nls++nrs) 0] : go ls rs
    | otherwise   = go [l] [] ++ go [] [r] ++ go ls rs
  goT [] [] = []
  goT [] (T d ts _ :rs) = T (dl+d) (genericReplicate dl epsilonTSym ++ ts) 0 : goT [] rs
  goT (T d ts _ :ls) [] = T (d+dr) (ts ++ genericReplicate dr epsilonTSym) 0 : goT ls []
  goT (T dll tsl _ :ls) (T drr tsr _ :rs) = T (dll+drr) (tsl++tsr) 0 : goT ls rs

