{-# LANGUAGE RecordWildCards #-}

-- | Render a grammar into a String, which may form the nucleus of a Haskell
-- module.
--
-- Note that the resulting file requires manual additions, such as the module
-- name.

module BioInf.GrammarProducts.Haskell where

import Data.Set (toList)
import Data.Function
import Control.Lens
import Data.List
import Text.Printf
import Data.Char

import BioInf.GrammarProducts.Grammar



renderGrammarHaskell :: [Grammar] -> String
renderGrammarHaskell = concat . intersperse "\n\n" . map rgh

-- render single grammar

rgh :: Grammar -> String
rgh g = concat $ intersperse "\n\n" [doctorsNote, hsGrammar g, renderSignature g]

-- Render just the rules as a multi-dim Haskell grammar. Individual memo-tables
-- and their fill functions are placed into an inductive tuple. The
-- @PrimitiveArray@ library has helper functions for filling such tuples of
-- tables.
--
-- TODO explicitly annotate the @""@ terminal symbol as the 'None' terminal
-- parser.

hsGrammar :: Grammar -> String
hsGrammar g = hdr ++ (concat $ inlined 2 $ [ "(Z:.\n" ] ++ intersperse i ts ++ [")" ]) ++ inl where
  hdr = printf "g%s S%s{..} {-non-terminals:-} %s {-terminals:-} %s\n" (g^.gname) (g^.gname) nbs tbs
  nbs = concat . intersperse " " . nub
      . concatMap (map mkNtSym . (^.lhs)) . toList $ g^.ps -- non-terminal binders
  tbs = concat . intersperse " " . filter (not . null)
      . nub . map (^.tname) . concatMap (^.symT) . filter isT
      . concatMap (^.rhs) . toList $ g^.ps -- terminal binders
  i = ":."
  ts = map mkProdRule xs
  xs = groupBy ((==) `on` (^.lhs)) . toList $ g^.ps
  inl = printf "\n{-# INLINE %s #-}\n" (g^.gname)

mkNtSym :: NtT -> String
mkNtSym t@T{..} = error $ "dying at finding terminal symbol in mkNtSym: " ++ show t
mkNtSym (Nt _ ns) = concatMap f ns
  where f (NTSym n 1 _) = printf "%s" (lowerHead n)
        f (NTSym n _ i) = printf "%s%d" (lowerHead n) i

mkProdRule :: [PR] -> String
mkProdRule ps@(p:_)
  | any (l/=) ls = error $ "found malformed production rule:" ++ show (p,ps)
  | otherwise    = "( " ++ concatMap mkNtSym l ++ " , " ++ mkRHS rs ++ " )\n"
  where ls = map (^.lhs) ps
        rs = ps
        l = p^.lhs

mkRHS :: [PR] -> String
mkRHS ps = (concat $ intersperse " ||| " $ map mkRule ps) ++ " ... h"

mkRule :: PR -> String
mkRule p = mkFunName (p^.fun) ++ " <<< " ++ (concat $ intersperse " % " $ map mkNtT $ p^.rhs)

mkNtT t@T{..} = "T:!" ++ (concat $ intersperse ":!" $ map mkSingleTSym (t^.symT))
mkNtT nt = mkNtSym nt

mkSingleTSym (TSym n)
  | null n = "None"
  | otherwise = n

lowerHead [] = []
lowerHead (x:xs) = toLower x : xs

inlined k xs = map (replicate k ' ' ++) xs

-- render the signature

renderSignature :: Grammar -> String
renderSignature g = printf "data S%s = S%s\n  { " (g^.gname) (g^.gname) ++ (concat $ intersperse "\n  , " xs) ++ "\n  }" where
  xs = map mkFunName . map (^.fun) . toList $ g^.ps

mkFunName = concat . intersperse "_"

-- render algebra product

doctorsNote = "\n-- TODO you now need to define the module name, the imports, as well as algebras and table-filling"
