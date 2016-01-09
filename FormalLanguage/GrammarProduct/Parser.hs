
-- | This parser extends the @FormalLanguage.Parser@ parser of single- and
-- multi-dim grammars to accept grammar product definitions as well.
--
-- TODO display function names like this: <fun,fun,fun>

module FormalLanguage.GrammarProduct.Parser where

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
import Data.List
import qualified Data.ByteString.Char8 as B
--import qualified Data.HashSet as H
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
--import Numeric.Natural.Internal
import Prelude hiding (subtract)
import Control.Monad
import Data.Char (isUpper)
import Data.Data.Lens
import System.IO.Unsafe (unsafePerformIO)

import FormalLanguage.CFG.Grammar
import FormalLanguage.CFG.Parser
import FormalLanguage.CFG.PrettyPrint.ANSI

import FormalLanguage.GrammarProduct.Op



-- TODO can remove, done via better FormalGrammars

-- -- | Parse a product grammar.
-- 
-- parseProduct :: String -> String -> Result [Grammar]
-- parseProduct fname cnts = parseString
--   ((evalStateT . runGrammarP) productParser def)
--   (Directed (B.pack fname) 0 0 0 0)
--   cnts
-- 
-- -- | Parse all grammars and grammar products, prepending to the list.
-- 
-- productParser = go [] <* eof where
--   go gs = do
--     whiteSpace
--     g' <- option Nothing $ Just <$> (try grammar <|> grammarProduct gs)
--     case g' of
--       Nothing -> return gs
--       Just g  -> go (g:gs)

-- | The top-level parser for a grammar product. It can be used as one of the
-- additional parser arguments, the formal grammars parser accepts.

parseGrammarProduct :: Parse m ()
parseGrammarProduct = do
  reserve fgIdents "Product:"
  n <- newGrammarName
  current <~ parseProductString
  current.grammarName .= n -- need to set here, otherwise the underlying combinator produces a combined name (funny but useless ;-)
  reserve fgIdents "//"
  v <- use verbose
  g <- use current
  seq (unsafePerformIO $ if v then (printDoc $ genGrammarDoc g) else return ())
    $ env %= M.insert n g

{-
grammarProduct gs = do
  reserveGI "Product:"
  n <- identGI
  r <- option Nothing $ Just <$> braces renameSymbols
  e <- getGrammar <$> expr (M.fromList [(g^.name,g) | g<-gs])
  reserveGI "//"
  return $ over (name) (const n) $ transformRenamed r e
-}

-- | Performs the actual parsing of a product string. Uses an expression parser
-- internally.

parseProductString :: Parse m Grammar
parseProductString = getGrammar <$> expr
  where expr :: Parse m ExprGrammar
        expr = buildExpressionParser table term
        table = [ [ binary "><"  exprDirect AssocLeft
                  , binary "*"   exprPower  AssocLeft
                  ]
                , [ binary "+"   exprPlus   AssocLeft
                  , binary "-"   exprMinus  AssocLeft
                  ]
                ]
        term =   parens expr
             <|> (ExprGrammar <$> knownGrammarName <?> "grammar not available in environment")
             <|> (ExprNumber  <$> natural <?> "integral power of grammar")
        binary n f a = Infix (f <$ reserve fgIdents n) a
        exprDirect l r = ExprGrammar (getGrammar l ><          getGrammar r)
        exprPlus   l r = ExprGrammar (getGrammar l `gAdd`      getGrammar r)
        exprMinus  l r = ExprGrammar (getGrammar l `gSubtract` getGrammar r)
        exprPower  l r = ExprGrammar (getGrammar l `gPower`    getNumber  r)

data ExprGrammar
  = ExprGrammar { getGrammar :: Grammar }
  | ExprNumber  { getNumber  :: Integer }

{-
expr :: Map String Grammar -> Parse ExprGrammar
expr g = e where
  e = buildExpressionParser table term
  table = [ [ binary "^><" highDirect AssocLeft
            ]
          , [ binary "><"  exprDirect AssocLeft
            , binary "*"   exprPower  AssocLeft
            ]
          , [ binary "+"   exprPlus   AssocLeft
            , binary "-"   exprMinus  AssocLeft
            ]
          ]
  term  =   parens e
        <|> (choice gts <?> "previously defined grammar")
        <|> (ExprNumber <$> natural <?> "integral power of grammar")
  gts = map (fmap ExprGrammar . gterm) $ M.assocs g
  binary n f a = Infix (f <$ reserveGI n) a
  exprDirect l r = ExprGrammar $ (getGrammar l >< getGrammar r)
  exprPlus   l r = ExprGrammar $ gAdd (getGrammar l) (getGrammar r)
  exprMinus  l r = ExprGrammar $ gSubtract (getGrammar l) (getGrammar r)
  exprPower  l r = ExprGrammar $ gPower (getGrammar l) (getNumber r)
  highDirect l r = error "highDirect (not active)!" -- ExprGrammar . unDirect $ times1p (Natural $ getNumber r -1) (Direct $ getGrammar l)

gterm :: (String,Grammar) -> Parse Grammar
gterm (s,g) = g <$ reserveGI s
-}

{-
transformRenamed Nothing  e = e
transformRenamed (Just r) e = go r e where
  go []     e = e
  go (RTN    f t:rs) e = go rs (e & tinplate %~ repTN   f t)
  go (RSymb  f t:rs) e = go rs (e & tinplate %~ repSymb f t)
  go (RFun   f t:rs) e = go rs (e & tinplate %~ repFun  f t)
  go (RStart s  :rs) e = go rs (repStart s e)
  repTN :: String -> String -> TN -> TN
  repTN f t r | r^.tnName == f = set tnName t r
  repTN _ _ r                  = r
  repSymb :: [String] -> [String] -> Symb -> Symb
  repSymb f t r | r^..symb.folded.tnName == f = Symb . map fixTN . zipWith (set tnName) t $ getSymbs r
  repSymb _ _ r = r
  fixTN r | r^.tnName == "ε" = E
  fixTN r = r
  repFun  f t r | r^.fun == f = set fun t r
  repFun  _ _ r               = r
  repStart [] e = set start Nothing         e
  repStart s  e = set start (Just . Symb . map (\z -> N z Singular) $ s) e

data Rename
  = RTN    String String -- one-dim term / non-term
  | RSymb  [String] [String] -- multi-dim symbol
  | RFun   [String] [String] -- replace function names
  | RStart [String] -- set or delete a start symbol

renameSymbols = (try rtn <|> rsymb <|> rfun <|> rstart) `sepBy` (symbol ",") where
  rtn    = RTN    <$> identGI <* string "->" <*> identGI
  rsymb  = RSymb  <$> (brackets $ identGI `sepBy` comma) <* string "->" <*> (brackets $ identGI `sepBy` comma)
  rfun   = RFun   <$> (angles   $ identGI `sepBy` comma) <* string "->" <*> (angles   $ identGI `sepBy` comma)
  rstart = RStart <$ string "S:" <*> (brackets $ identGI `sepBy` comma)

-}




















{-
data GS = GS
  { _ntsyms     :: Map String Integer
  , _tsyms      :: Set String
  , _gs         :: Map String Grammar
  , _gCount     :: Integer
  , _grammarUid :: Integer
  }
  deriving (Show)

instance Default GS where
  def = GS
    { _ntsyms     = def
    , _tsyms      = def
    , _gs         = def
    , _gCount     = def
    , _grammarUid = def
    }

makeLenses ''GS

-- | Parsing product expressions, producing a grammar, again

{-
expr :: Map String Grammar -> Parse Grammar
expr g = choice [directprod] where
  directprod = do
    gl <- choice gts
    reserve gi "><"
    gr <- choice gts
    return . unDirect $ Direct gl <> Direct gr
  gts = map gterm $ M.assocs g
-}

expr :: Map String Grammar -> Parse ExprGrammar
expr g = e where
  e = buildExpressionParser table term
  table = [ [ binary "^><" highDirect AssocLeft
            ]
          , [ binary "><"  exprDirect AssocLeft
            , binary "*"   exprPower  AssocLeft
            ]
          , [ binary "+"   exprPlus   AssocLeft
            , binary "-"   exprMinus  AssocLeft
            ]
          ]
  term  =   parens e
        <|> (choice gts <?> "previously defined grammar")
        <|> (ExprNumber <$> natural <?> "integral power of grammar")
  gts = map (fmap ExprGrammar . gterm) $ M.assocs g
  binary n f a = Infix (f <$ reserve gi n) a
  exprDirect l r = ExprGrammar . unDirect $ (Direct $ getGrammar l) <> (Direct $ getGrammar r)
  exprPlus   l r = ExprGrammar . unAdd $ (Add $ getGrammar l) <> (Add $ getGrammar r)
  exprMinus  l r = ExprGrammar $ subtract (getGrammar l) (getGrammar r)
  exprPower  l r = ExprGrammar $ power (getGrammar l) (getNumber r)
  highDirect l r = ExprGrammar . unDirect $ times1p (Natural $ getNumber r -1) (Direct $ getGrammar l)

data ExprGrammar
  = ExprGrammar { getGrammar :: Grammar }
  | ExprNumber  { getNumber  :: Integer }

gterm :: (String,Grammar) -> Parse Grammar
gterm (s,g) = g <$ reserve gi s

-- | Grammar product

gprod :: Parse Grammar
gprod = do
  reserve gi "Product:"
  n <- ident gi
  g <- use gs
  e <- getGrammar <$> expr g
  reserve gi "//"
  let g = e & gname .~ n
  gs <>= M.singleton (g ^. gname) g
  return g

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
  gCount += 1
  -- begin parsing
  reserve gi "Grammar:"
  n <- ident gi
  (nts,ts) <- partitionEithers <$> ntsts
  rs <- concat <$> some rule
  reserve gi "//"
  let g = Grammar (S.fromList rs) n
  gs <>= M.singleton (g ^. gname) g
  return g

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
  let ret = runReaderT (genPR fun ln i zs) s
  return ret

-- | Generate one or more production rules from a parsed line.

genPR :: String -> String -> NtIndex -> [Either (String,NtIndex) String] -> ReaderT GS [] PR
genPR f ln i xs = go where
  go = do
    (l,(m,k)) <- genL i
    r <- genR m k xs
    return $ PR [l] r [f]
  genL NoIdx = do
    g <- view grammarUid
    return (Nt 1 [NTSym ln 1 0], (1,0))
  genL (WithVar v 0) = do
    g <- view grammarUid
    m <- views ntsyms (M.! ln)
    k <- lift [0 .. m-1]
    return (Nt 1 [NTSym ln m k], (m,k))
  genL (Range xs) = do
    g <- view grammarUid
    m <- views ntsyms (M.! ln)
    k <- lift xs
    return (Nt 1 [NTSym ln m k], (m,k))
  genR m k [] = do
    return []
  genR m k (Left (n,WithVar k' p) :rs) = do
    let (WithVar v 0) = i
    g <- view grammarUid
    nm <- views ntsyms (M.! n)
    when (v/=k') $ error "oops, index var wrong"
    rs' <- genR m k rs
    return (Nt 1 [NTSym n m ((k+p) `mod` m)] :rs')
  genR m k (Left (n,Range ls) :rs) = do
    g <- view grammarUid
    nm <- views ntsyms (M.! n)
    l <- lift ls
    rs' <- genR m k rs
    return (Nt 1 [NTSym n m l] :rs')
  genR m k (Left (n,NoIdx) :rs) = do
    g <- view grammarUid
    nm <- views ntsyms (M.! n)
    when (nm>1) $ error $ printf "oops, NoIdx given, but indexed NT in: %s" (show (nm,m,k,n,rs))
    rs' <- genR m k rs
    return (Nt 1 [NTSym n 1 0] :rs')
  genR m k (Right t :rs) = do
    g <- view grammarUid
    rs' <- genR m k rs
    return (T 1 [TSym t] :rs')

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
  whiteSpace
  {-
  gs <- some grammar
  let g = undefined -- M.fromList $ map ((^. gname) &&& id) gs
  ps <- some (gprod g)
  -}
  gsps <- some (grammar <|> gprod)
  eof
  let (gs,ps) = partition ((==1) . grammarDim) gsps
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
-}

