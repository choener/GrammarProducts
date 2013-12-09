
module FormalLanguage.GrammarProduct.Op.Chomsky.Proof where

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

import FormalLanguage.CFG.Grammar
import FormalLanguage.CFG.PrettyPrint.ANSI
import FormalLanguage.CFG.PrettyPrint.LaTeX
import FormalLanguage.CFG.Parser

import FormalLanguage.GrammarProduct.Op.Chomsky



-- * Proof of associativity of the 2-GNF.

-- | Run the 2-gnf grammar with the TwoGNF monoid which observes the 2 star
-- cases.

cNFassociativity :: (Grammar, Grammar, S.Set Rule, S.Set Rule, Bool)
cNFassociativity = ( l
                   , r
                   , (l^.rules) S.\\ (r^.rules)
                   , (r^.rules) S.\\ (l^.rules)
                   , l^.rules == r^.rules)  where
  l = runCNF $ (CNF g <>  CNF g) <> CNF g
  r = runCNF $  CNF g <> (CNF g  <> CNF g)
  g = cNFgrammar

cNFs = g where
  g = runCNF $ (CNF h <> CNF h)
  h = cNFgrammar

showTwo = printDoc $ grammarDoc $ runCNF  $ CNF cNFgrammar <> CNF cNFgrammar

-- * The simple 2-gnf grammar to run the proof on.

-- | Very simple 2-gnf form for proofs.

cNFgrammar = case g of
  Success g' -> g'
  Failure f  -> error $ show f
  where
  g = parseGrammar "testGrammar" twoGNF
  twoGNF = unlines
    [ "Grammar: CNF"
    , "N: A"
    , "N: B"
    , "N: C"
--    , "N: Sa"
    , "T: a"
    , "A  -> twoN <<< B C"
    , "A  -> oneT <<< a"
--    , "A  -> oneN <<< Sa"
--    , "Sa -> oneT <<< a"
    , "//"
    ]

