{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module BioInf.GrammarProducts.Parser where

import Control.Arrow
import Control.Applicative
import Control.Lens
import Control.Monad (MonadPlus(..), guard, when)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Reader
import Data.Default
import Data.Either
import Data.Map (Map)
import Data.Set (Set)
import Debug.Trace
import qualified Data.ByteString as B
import qualified Data.HashSet as H
import qualified Data.List.NonEmpty as N
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parser.Expression
import Text.Parser.Token.Highlight
import Text.Parser.Token.Style
import Text.Printf
import Text.Trifecta
import Text.Trifecta.Delta
import Text.Trifecta.Result
import Data.Semigroup ((<>))
import qualified Control.Newtype as T

import BioInf.GrammarProducts.Grammar
import BioInf.GrammarProducts.Op.Direct



data GS = GS
  { _ntsyms     :: Map String Integer
  , _tsyms      :: Set String
  , _gs         :: Int
  , _grammarUid :: Integer
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

-- | Parsing product expressions, producing a grammar, again

expr :: Map String Grammar -> Parse Grammar
expr g = choice [directprod] where
  directprod = do
    gl <- choice gts
    reserve gi "><"
    gr <- choice gts
    return . unDirect $ Direct gl <> Direct gr
  gts = map gterm $ M.assocs g

expr' :: Parse ExprGrammar
expr' = e where
  e = buildExpressionParser table term
  table = [ [ binary "><" exprDirect AssocLeft ]
          ]
  term  =   parens e
        <|> choice gts
  gts = map gterm $ M.assocs g
  binary n f a = Infix (f <$ reserve gi n) a
  exprDirect = 

data ExprGrammar
  = ExprGrammar { getGrammar :: Grammar }
  | ExprNumber  Integer

{-
expr :: Map String Grammar -> Parse Grammar
expr g = e where 
  e = buildExpressionParser table term
  table = [ [ binary "><" zzz AssocLeft ]
          ]
  term =   parens e
       <|> choice gts
       <|> (yyy <$> term <* reserve gi "^><" <*> natural)
  gts = map gterm $ M.assocs g
  binary n f a = Infix (f <$ reserve gi n) a
  zzz :: Grammar -> Grammar -> Grammar
  zzz = undefined
  yyy :: Grammar -> Integer -> Grammar
  yyy = undefined
-}

gterm :: (String,Grammar) -> Parse Grammar
gterm (s,g) = do
  reserve gi s
  return g

-- | Grammar product

gprod :: Map String Grammar -> Parse Grammar
gprod g = do
  reserve gi "Product:"
  n <- ident gi
  e <- expr g
  reserve gi "//"
  return $ e & gname .~ n

data Product = Product
  deriving (Show)

-- |
--
-- TODO complain on indexed NTs with modulus '1'

grammar :: Parse Grammar
grammar = do
  -- reset some information
  ntsyms .= def
  tsyms  .= def
  -- new grammar
  gs += 1
  -- begin parsing
  reserve gi "Grammar:"
  n <- ident gi
  (nts,ts) <- partitionEithers <$> ntsts
  rs <- concat <$> some rule
  reserve gi "//"
  return $ Grammar (S.fromList rs) n

-- | Parse a single rule. Some rules come attached with an index. In that case,
-- each rule is inflated according to its modulus.
--
-- TODO add @fun@ to each PR

rule :: Parse [PR]
rule = do
  ln <- ident gi <?> "rule: lhs non-terminal"
  uses ntsyms (M.member ln) >>= guard <?> (printf "undeclared NT: %s" ln)
  i <- nTindex
  reserve gi "->"
  fun <- ident gi
  reserve gi "<<<"
  zs <- runUnlined $ some (Left <$> try ruleNts <|> Right <$> try ruleTs)
  whiteSpace
  s <- get
  let ret = runReaderT (genPR ln i zs) s
  return ret

-- | Generate one or more production rules from a parsed line.

genPR :: String -> NtIndex -> [Either (String,NtIndex) String] -> ReaderT GS [] PR
genPR ln i xs = go where
  go = do
    (l,(m,k)) <- genL i
    r <- genR m k xs
    return $ PR [l] r
  genL NoIdx = do
    g <- view grammarUid
    return (Nt 1 [NTSym ln 1 0] g, (1,0))
  genL (WithVar v 0) = do
    g <- view grammarUid
    m <- views ntsyms (M.! ln)
    k <- lift [0 .. m-1]
    return (Nt 1 [NTSym ln m k] g, (m,k))
  genL (Range xs) = do
    g <- view grammarUid
    m <- views ntsyms (M.! ln)
    k <- lift xs
    return (Nt 1 [NTSym ln m k] g, (m,k))
  genR m k [] = do
    return []
  genR m k (Left (n,WithVar k' p) :rs) = do
    let (WithVar v 0) = i
    g <- view grammarUid
    nm <- views ntsyms (M.! n)
    when (v/=k') $ error "oops, index var wrong"
    rs' <- genR m k rs
    return (Nt 1 [NTSym n m ((k+p) `mod` m)] g :rs')
  genR m k (Left (n,Range ls) :rs) = do
    g <- view grammarUid
    nm <- views ntsyms (M.! n)
    l <- lift ls
    rs' <- genR m k rs
    return (Nt 1 [NTSym n m l] g :rs')
  genR m k (Left (n,NoIdx) :rs) = do
    g <- view grammarUid
    nm <- views ntsyms (M.! n)
    when (nm>1) $ error $ printf "oops, NoIdx given, but indexed NT in: %s" (show (nm,m,k,n,rs))
    rs' <- genR m k rs
    return (Nt 1 [NTSym n 1 0] g :rs')
  genR m k (Right t :rs) = do
    g <- view grammarUid
    rs' <- genR m k rs
    return (T 1 [TSym t] g :rs')

ruleNts :: ParseU (String,NtIndex)
ruleNts = do
  n <- ident gi <?> "rule: nonterminal identifier"
  i <- nTindex <?> "rule:" -- option ("",1) $ braces ((,) <$> ident gi <*> option 0 integer) <?> "rule: nonterminal index"
  lift $ uses ntsyms (M.member n) >>= guard <?> (printf "undeclared NT: %s" n)
  return (n,i)

nTindex :: ParseG NtIndex
nTindex = option NoIdx
  $   try (braces $ WithVar <$> ident gi <*> option 0 integer)
  <|> try (Range <$> braces (commaSep1 integer))
  <?> "non-terminal index"

data NtIndex
  = WithVar String Integer
  | Range [Integer]
  | NoIdx
  deriving (Show)

ruleTs :: ParseU String
ruleTs = do
  n <- ident gi <?> "rule: terminal identifier"
  lift $ uses tsyms (S.member n) >>= guard <?> (printf "undeclared T: %s" n)
  return n

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

parseDesc = do
  gs <- some grammar
  let g = M.fromList $ map ((^. gname) &&& id) gs
  ps <- some (gprod g)
  eof
  return (gs,ps)

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
type ParseU a = (Monad m, TokenParsing m, MonadPlus m) => Unlined (StateT GS m) a
type ParseG a = (Monad m, TokenParsing m, MonadPlus m) => m a

instance MonadTrans Unlined where
  lift = Unlined
  {-# INLINE lift #-}

