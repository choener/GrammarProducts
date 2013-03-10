{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE QuasiQuotes #-}

-- | TODO we need to fix the start symbol (not that important, from whatever
-- non-terminal we read the final result, make said non-terminal the start
-- symbol.

module Main where

import qualified Language.Haskell.TH as TH
import TupleTH

import BioInf.GrammarProducts
import BioInf.GrammarProducts.TH

main :: IO ()
main = return ()

--x = $(pr "x")

--hmm :: String
-- hmm = [qqGV|

-- Define the grammar "Test1" with one non-terminal X and two terminals "a" and
-- "-". The Product will be named "Prod" (and the emitted symbol will be
-- "gProd". We remove one symbol from the final grammar, namely the terminal
-- made up of only deletions. All production rules with just that symbol will
-- be deleted as well.
--
-- TODO add algebra functions "X -> .f X a" .. if we use only one function ".f"
-- and keep that one polymorphic, as down below, we should be happy enough as
-- the product op should generate only one function ".ffff" for us.


[qqGDhere|
Grammar: Test1
N: X
T: p
T: a
T: -
F: f
F: g
X -> f $ X p a
X -> g $ X p -
//
Product: Prod
Prod: Test1 * Test1 * Test1 * Test1
remove: -,-,-,-
//
|]


allTuples = $(subtuples 4 2)
{-
foldTuples = $(foldlTuple 6)
sumOfPairs f x = foldTuples f x . allTuples
-}

type LookupTable = () -- how to look up?

-- | we probably want to add "x" later, not in each pairEval function ?!  On
-- the other hand, this is more like the usual algebra stuff, especially with
-- regards to backtracking ...

class PairEval a b where
  pairEval :: LookupTable -> Int -> (a,b) -> Int

instance PairEval Int Int where
  pairEval lkup x (a,b) = x + (a+b) -- replace (a+b) with "lkup a b"

instance PairEval Int () where
  pairEval lkup x (a,()) = x + (a)

instance PairEval () Int where
  pairEval lkup x ((),a) = x + (a)

instance PairEval () () where
  pairEval lkup x ((),()) = x + 0

allPE lkup x t = $(sumTuple 6) $ pe $ allTuples t where
  pe (a,b,c,d,e,f) =
    ( pairEval lkup x a
    , pairEval lkup x b
    , pairEval lkup x c
    , pairEval lkup x d
    , pairEval lkup x e
    , pairEval lkup x f
    )

--finalSOP = sumOfPairs (pairEval (undefined :: LookupTable))

i :: Int
i = 1









{-

-- We want and need attributes (f,g here) to be able to identify algebra
-- functions we need to create. Maybe we can do it without explicit naming?
-- More of a U/I questions.
Grammar: Test_1
N: X
T: a
F: _f _g

X -> _f < a X a | _g < X a

-}

{-
test = [grammar|

-- "-" aligns with terminals (doesn't introduce a new terminal hole), but
-- modifies the vectorial terminal in the respective dimension to not advance.
-- data VC <ynm> <xs>, where ynm = Yes|No|Maybe. Yes advances, No doesn't
-- advance, maybe produces two cases, one advancing, one not. And since these
-- are empty data decls, they are optimized away during compilation.
Grammar: AlignAffine

N: X Y
T: u -

X -> X u
X -> Y u

Y -> X -
Y -> Y -

|]
-}

-- example we want to be able to parse

{-

-- comments "--" should be comments

-- simple productions. All uppercase strings provide non-terminals. All
-- lower-case strings provide terminals. Different strings produce different
-- symbols. This grammar will yield an ADPfusion grammar with "holes" for X,Y
-- non-terminals. It will also provide "holes" for uu, u, v, z terminals. The
-- special terminal "-" takes a normal character parser (like "Chr" from
-- ADPfusion) and allows this parser to advance 0 characters. This is only
-- really useful in higher-dimensional parsing, where we want to introduce
-- in-del's

X -> X uu
X -> u X
X -> u X v
X -> u X v Y z

Y -> X u | Y u

-}

{-

-- In addition we want attributes for our grammars. <Char> is of type
-- Z:.Char:.Char:.Char for 3-cross, and Z:.Char:.Char for 2-sequence alignment.

X -> X u        match :: \xi -> <Char> -> \xi  || \xi is the E-type of the non-terminal table (here: Int)
X -> Y u

Y -> X -
Y -> Y -

-}

{-

-- the 2d version

X ->  Xu  |  Xu  |  Yu  |  Yu          ||   f X1X2 <u1,u2> = X1X2 + match (u1,u2)
X     Xu  |  Yu  |  Xu  |  Yu          ||   f :: Int -> <C,C> -> Int

X ->  Xu  |  Xu  |  Yu  |  Yu
Y     X-  |  Y-  |  X-  |  Y-

Y ->  X-  |  X-  |  Y-  |  Y-
X     Xu  |  Yu  |  Xu  |  Yu

Y ->  X-  |  X-  |  Y-  |  Y-
Y     X-  |  Y-  |  X-  |  Y-

-}



{-

N: F^[0..2 | 3]  -- these are three non-terminals, indexed by ints. the (| 3) "mod 3" operator leads to wrap-around
T: u v w .

F^i  ->  F^i u v w  |  F^i . . .    -- one should remove productions where the RHS contains only "delete" terminals
F^i  ->  F^(i+1) . v w
F^i  ->  F^(i+2) . . w

===

N: P
T: a .

P  ->  P a | P .

-}
