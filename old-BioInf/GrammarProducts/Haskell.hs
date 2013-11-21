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
rgh g = concat $ intersperse "\n\n" [renderSignature g, hsGrammar g]

-- Render just the rules as a multi-dim Haskell grammar. Individual memo-tables
-- and their fill functions are placed into an inductive tuple. The
-- @PrimitiveArray@ library has helper functions for filling such tuples of
-- tables.
--
-- TODO explicitly annotate the @""@ terminal symbol as the 'None' terminal
-- parser.

hsGrammar :: Grammar -> String
hsGrammar g = hdr ++ (concat $ inlined 2 $ [ "(Z:.\n" ] ++ intersperse i ts ++ [")" ]) ++ inl where
  hdr = printf "g%s s%s {-non-terminals:-} %s {-terminals:-} %s =\n" (g^.gname) (g^.gname) nbs tbs
  nbs = concat . intersperse " " . nub
      . concatMap (map mkNtSym . (^.lhs)) . toList $ g^.ps -- non-terminal binders
  tbs = concat . intersperse " "
      . nub . concatMap mkTSym . filter isT
      . concatMap (^.rhs) . toList $ g^.ps -- terminal binders
  i = ":."
  ts = map (mkProdRule (printf "s%s" (g^.gname))) xs
  xs = groupBy ((==) `on` (^.lhs)) . toList $ g^.ps
  inl = printf "\n{-# INLINE g%s #-}\n" (g^.gname)

mkTSym :: NtT -> [String]
mkTSym nt@Nt{..} = error $ "dying at finding non-terminal symbol in mkTSym: " ++ show nt
mkTSym (T _ ts)  = zipWith (\(TSym t) n -> printf "%s_%d" t n) (filter (not . null . (^.tname)) ts) [1 :: Int ..]

mkNtSym :: NtT -> String
mkNtSym t@T{..} = error $ "dying at finding terminal symbol in mkNtSym: " ++ show t
mkNtSym (Nt _ ns) = concatMap f ns
  where f (NTSym n 1 _) = printf "%s" (lowerHead n)
        f (NTSym n _ i) = printf "%s%d" (lowerHead n) i

mkProdRule :: String -> [PR] -> String
mkProdRule c ps@(p:_)
  | any (l/=) ls = error $ "found malformed production rule:" ++ show (p,ps)
  | otherwise    = "( " ++ concatMap mkNtSym l ++ " , " ++ mkRHS c rs ++ " )\n"
  where ls = map (^.lhs) ps
        rs = ps
        l = p^.lhs

mkRHS :: String -> [PR] -> String
mkRHS c ps = (concat $ intersperse " ||| " $ map (mkRule c) ps) ++ " ... h " ++ c

mkRule :: String -> PR -> String
mkRule c p = mkFunName (p^.fun) ++ " " ++ c ++ " <<< " ++ (concat $ intersperse " % " $ map mkNtT $ p^.rhs)

mkNtT t@T{..} = "(T:!" ++ (concat $ intersperse ":!" $ zipWith mkSingleTSym' [1::Int ..] (t^.symT)) ++ ")"
mkNtT nt = mkNtSym nt

mkSingleTSym' d (TSym t)
  | null t = "None"
  | otherwise = printf "%s_%d" t d

mkSingleTSym (TSym n)
  | null n = "()"
  | otherwise = n

lowerHead [] = []
lowerHead (x:xs) = toLower x : xs

inlined k xs = map (replicate k ' ' ++) xs

-- render the signature

renderSignature :: Grammar -> String
renderSignature g = printf "data S%s _m _x _r %s = S%s\n  { "
                            (g^.gname) ts (g^.gname) ++ (concat $ intersperse "\n  , " xs) ++ "\n  }"
  where
    xs = (map mkFunType . toList $ g^.ps) ++ ["h :: Stream _m _x -> _m _r"]
    ts = concat . intersperse " " . nub 
       . filter (not . null) . map (^.tname)
       . concatMap (^.symT) . filter isT
       . concatMap (^.rhs) . toList $ g^.ps

mkFunName = concat . intersperse "_"

mkFunType p = (concat . intersperse "_" $ p^.fun) ++ " :: " ++ as where
  as = (concat $ intersperse " -> " $ map trans $ p^.rhs) ++ " -> _x"
--  trans t@T{..} = "(Z:." ++ (concat $ intersperse ":." $ map mkSingleTSym (t^.symT)) ++ ")"
  trans t@T{..} = foldl' (\s n -> concat ["(",s,":.",n,")"]) "Z" $ map mkSingleTSym (t^.symT)
  trans nt = "_x" -- mkNtSym nt

-- render algebra product

doctorsNote = "\n-- TODO you now need to define the module name, the imports, as well as algebras and table-filling"
