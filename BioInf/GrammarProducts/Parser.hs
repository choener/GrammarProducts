{-# LANGUAGE StandaloneDeriving #-}
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
import Control.Monad (MonadPlus(..), guard)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Default

import BioInf.GrammarProducts.Grammar



data GS = GS
  { _ntsyms     :: Map String Integer
  , _tsyms      :: Set String
  , _gs         :: Int
  , _grammarUid :: Int
  }
  deriving (Show)

instance Default GS where
  def = GS
    { _ntsyms     = def
    , _tsyms      = def
    , _gs         = def
    , _grammarUid = def
    }

makeLenses ''GS



-- |
--
-- TODO complain on indexed NTs with modulus '1'

grammar :: Parse String
grammar = do
  -- reset some information
  ntsyms .= def
  tsyms  .= def
  -- begin parsing
  reserve gi "Grammar:"
  n <- ident gi
  (nts,ts) <- partitionEithers <$> ntsts
  rs <- some rule
  reserve gi "//"
  error $ show (n,nts,ts,rs)
  return n

-- | Parse a single rule. Some rules come attached with an index. In that case,
-- each rule is inflated according to its modulus.

rule :: Parse String
rule = do
  ln <- ident gi <?> "rule: lhs non-terminal"
  mi <- optional $ braces $ ident gi
  reserve gi "->"
  fun <- ident gi
  reserve gi "<<<"
  zs <- runUnlined $ partitionEithers <$> some (Left <$> try ruleNts <|> Right <$> try ruleTs)
  whiteSpace
  return $ show (ln,mi,fun,zs)

ruleNts :: ParseUnlined String
ruleNts = do
  n <- ident gi <?> "rule: nonterminal identifier"
  mi <- optional $ braces ((,) <$> ident gi <*> option 0 integer) <?> "rule: nonterminal index"
  lift $ uses ntsyms (M.member $ n) >>= guard
  return $ show (n,mi)

ruleTs :: ParseUnlined String
ruleTs = do
  n <- ident gi <?> "rule: terminal identifier"
  lift $ uses tsyms (S.member n) >>= guard
  return $ show (n)

ntsts :: Parse [Either NTSym TSym]
ntsts = concat <$> some (map Left <$> nts <|> map Right <$> ts)

-- |
--
-- TODO expand @NT@ symbols here or later?

nts :: Parse [NTSym]
nts = do
  reserve gi "NT:"
  n <- ident gi
  mdl <- option 1 $ braces natural
  let zs = map (NTSym n mdl) [0 .. mdl-1]
  ntsyms <>= M.singleton n mdl
  return zs

ts :: Parse [TSym]
ts = do
  reserve gi "T:"
  n <- ident gi
  let z = TSym n
  tsyms <>= S.singleton n
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
type ParseUnlined a = (Monad m, TokenParsing m, MonadPlus m) => Unlined (StateT GS m) a

instance MonadTrans Unlined where
  lift = Unlined
  {-# INLINE lift #-}

