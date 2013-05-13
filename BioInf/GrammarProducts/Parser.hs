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

import BioInf.GrammarProducts.Grammar



-- | Include 'StateT' that lets use store the modulus for each NT
--
-- TODO complain on indexed NTs with modulus '1'

grammar :: (Monad m, TokenParsing m) => m String
grammar = do
  reserve gi "Grammar:"
  n <- ident gi
  (nts,ts) <- partitionEithers <$> ntsts
  return n

ntsts :: (Monad m, TokenParsing m) => m [Either () ()]
ntsts = some (Left <$> nts <|> Right <$> ts)

-- |
--
-- TODO expand @NT@ symbols here or later?

nts :: (Monad m, TokenParsing m) => m ()
nts = do
  reserve gi "NT:"
  n <- ident gi
  mdl <- option 1 $ braces natural
  return ()

ts :: (Monad m, TokenParsing m) => m ()
ts = do
  reserve gi "T:"
  n <- ident gi
  return ()


test :: IO ()
test = do
  gs <- parseFromFile (runGrammarLang $ some grammar <* eof) "./tests/protein.gra"
  print gs

gi = set styleReserved rs emptyIdents where
  rs = H.fromList ["Grammar:", "NT:", "T:"]

newtype GrammarLang m a = GrammarLang { runGrammarLang :: m a }
  deriving (Functor,Applicative,Alternative,Monad,MonadPlus,Parsing,CharParsing)

instance MonadTrans GrammarLang where
  lift = GrammarLang
  {-# INLINE lift #-}

instance TokenParsing m => TokenParsing (GrammarLang m) where
  someSpace = GrammarLang $ someSpace `buildSomeSpaceParser` haskellCommentStyle
