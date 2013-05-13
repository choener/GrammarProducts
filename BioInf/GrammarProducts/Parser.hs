{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module BioInf.GrammarProducts.Parser where

import Control.Applicative
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Trifecta.Result
import Data.Either
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import qualified Data.ByteString as B
import Control.Lens
import qualified Data.HashSet as H
import Control.Monad (MonadPlus(..))
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as S
import Data.Default

import BioInf.GrammarProducts.Grammar



data GS = GS
  { _ntsyms :: Set NTSym
  , _tsyms  :: Set TSym
  , _gs :: Int
  }
  deriving (Show)

instance Default GS where
  def = GS
    { _ntsyms = def
    , _tsyms  = def
    , _gs     = def
    }

makeLenses ''GS



-- |
--
-- TODO complain on indexed NTs with modulus '1'

grammar :: Parse String
grammar = do
  reserve gi "Grammar:"
  n <- ident gi
  (nts,ts) <- partitionEithers <$> ntsts
--  rs <- rules
  return n

rules :: (Monad m, TokenParsing m) => m [()]
rules = undefined

ntsts :: Parse [Either NTSym TSym]
ntsts = concat <$> some (map Left <$> nts <|> map Right <$> ts)

-- |
--
-- TODO expand @NT@ symbols here or later?

nts :: Parse [NTSym]
nts = do
  reserve gi "NT:"
  n <- ident gi
  mdl <- option 1 $ braces (fromIntegral <$> natural)
  let zs = map (NTSym n mdl) [0 .. mdl-1]
  ntsyms <>= S.fromList zs
  return zs

ts :: Parse [TSym]
ts = do
  reserve gi "T:"
  n <- ident gi
  let z = TSym n
  tsyms <>= S.singleton z
  return [z]


test :: IO ()
test = do
  gs <- parseFromFile (runGrammarLang $ flip evalStateT def $ some grammar <* eof) "./tests/protein.gra"
  print gs

gi = set styleReserved rs emptyIdents where
  rs = H.fromList ["Grammar:", "NT:", "T:"]

newtype GrammarLang m a = GrammarLang {runGrammarLang :: m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,Parsing,CharParsing)

instance MonadTrans GrammarLang where
  lift = GrammarLang
  {-# INLINE lift #-}

instance TokenParsing m => TokenParsing (GrammarLang m) where
  someSpace = GrammarLang $ someSpace `buildSomeSpaceParser` haskellCommentStyle

type Parse a = (Monad m, TokenParsing m, MonadPlus m) => StateT GS m a

