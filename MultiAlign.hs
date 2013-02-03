{-# LANGUAGE QuasiQuotes #-}

module Main where

import BioInf.GrammarProducts

main :: IO ()
main = return ()


{-
test = [grammar|

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
