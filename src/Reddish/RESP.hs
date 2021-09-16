module Reddish.RESP where

import Prelude hiding (String, Integer, length, null)
import qualified Prelude (String, Integer, length)

import Control.Applicative (empty, liftA2)
import Control.Arrow ((>>>), first)
import Control.Monad (guard, join)
import Data.Traversable (for)
import Data.Binary (Binary, get, put)
import Data.Binary.Put (putBuilder, putLazyByteString)
import Data.Binary.Get (getInt64host, getWord8, lookAhead, runGet)
import Data.Binary.Get.Internal (Consume, Get, failOnEOF, getByteString, runCont, withInputChunks)
import Data.Bits (toIntegralSized)
import qualified Data.ByteString as B
  (ByteString, break, drop, elem, empty, find, foldl', length, null, singleton, splitAt, stripPrefix, span, take)
import Data.ByteString.Builder (Builder, int64Dec, toLazyByteString)
import Data.ByteString.Builder.Extra (int64Host)
import Data.ByteString.Internal (w2c)
import Data.ByteString.Lazy (ByteString, fromChunks, fromStrict, length, toStrict)
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Data.Char (isDigit, digitToInt)
import Data.Either.Combinators (maybeToRight)
import Data.Foldable (find)
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Int (Int64)
import Data.Kind (Constraint, Type)
import Data.Maybe (fromMaybe, isJust, maybe)
import Data.Monoid (getSum)
import Data.Proxy (pattern Proxy)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Type.Equality (type (==))
import Data.Vinyl.Derived (HList)
import Data.Vinyl.Core (Dict(..), RFoldMap, RMap, RecordToList, Rec(..), ReifyConstraint, recordToList, reifyConstraint, rfoldMap)
import Data.Vinyl.Functor (Identity(..), Compose(..), getCompose, getIdentity)
import Data.Vinyl.TypeLevel (RecAll, AllConstrained)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)

--
-- RESP eDSL
--

newtype String = String_ { unString_ :: Prelude.String }
  deriving stock (Generic, Show)

-- | smart constructor
pattern String :: HasCallStack => Prelude.String -> String
pattern String { unString } <- String_ !unString where
  String str = if isJust $ find (\char -> char `elem` unpack crlf) str
  then error $ "RESP Simple String must not contain line breaks: " <> show str
  else String_ str

newtype Error = Error_ { unError_ :: Prelude.String }
  deriving stock (Generic, Show)

-- | smart constructor
pattern Error :: HasCallStack => Prelude.String -> Error
pattern Error { unError } <- Error_ !unError where
  Error str = if isJust $ find (\char -> char `elem` unpack crlf) str
  then error $ "RESP Error must not contain line breaks: " <> show str
  else Error_ str

newtype Integer = Integer_ { unInteger_ :: Int64 }
  deriving stock (Generic, Show)

-- | smart constructor
pattern Integer { unInteger } = Integer_ unInteger

newtype BulkString = BulkString_ { unBulkString_ :: Maybe Text }
  deriving stock (Generic, Show)

-- | smart constructor
pattern BulkString :: HasCallStack => Maybe Text -> BulkString
pattern BulkString { unBulkString } <- BulkString_ !unBulkString where
  BulkString = \case
    Just txt | sizeLimit < B.length (encodeUtf8 txt) ->
      error $ "RESP Bulk String must be up to 512 Mb: " <> show txt
    payload -> BulkString_ payload
    where
    sizeLimit = 536_870_912 -- 512 Mb bytes count

data Array :: [RESP] -> Type where
  Array_ :: forall (elemTypes :: [RESP]) (elems :: [Type])
    . (elemTypes ~ MapToKind elems, elems ~ MapFromKind elemTypes)
    => { unArray_ :: !(HList elems) } -> Array elemTypes
  ArrayNull_ :: Array '[]

-- ???: is maxBound @Word or @Int64 -- the maximum array size
-- | smart constructor
pattern Array :: forall (elemTypes :: [RESP]) (elems :: [Type])
  . (elemTypes ~ MapToKind elems, elems ~ MapFromKind elemTypes)
  =>  HList elems -> Array elemTypes
pattern Array { unArray } = Array_ unArray

-- | smart constructor
pattern ArrayNull :: forall (elems :: [RESP])
  . ()
  => (elems ~ '[])
  => Array elems
pattern ArrayNull = ArrayNull_

deriving instance forall (elems :: [RESP])
  . The (MapFromKind elems) '[RMap
  , ReifyConstraint Show Identity
  , RecordToList] => Show (Array elems)

-- existential wrapper for the Array
data SomeArray :: Type where
  SomeArray_ :: forall (elemTypes :: [RESP]) (elems :: [Type])
    . (elemTypes ~ MapToKind elems
      , elems ~ MapFromKind elemTypes
      , Terms elems)
    => !(Maybe (HList elems)) -> SomeArray

-- ???: is maxBound @Word or @Int64 -- the maximum array size
-- | smart constructor
pattern SomeArray
  :: ()
  => (elemTypes ~ MapToKind elems, elems ~ MapFromKind elemTypes
    , Terms elems)
  =>  Maybe (HList elems) -> SomeArray
pattern SomeArray unSomeArray = SomeArray_ unSomeArray

deriving instance Show SomeArray

type Terms :: [Type] -> Constraint
type Terms elems = The elems '[RFoldMap, ReifyConstraint ToTerm Identity
  , RMap, ReifyConstraint Show Identity, RecordToList]

class ToTerm term where
  toTerm :: term -> Term (ToKind term)
instance ToTerm String where toTerm = TermString
instance ToTerm Error where toTerm = TermError
instance ToTerm Integer where toTerm = TermInteger
instance ToTerm BulkString where toTerm = TermBulkString
instance forall (elemTypes :: [RESP]) (elems :: [Type])
  . (elemTypes ~ MapToKind elems, elems ~ MapFromKind elemTypes
    , Terms elems)
  => ToTerm (Array elemTypes) where toTerm = TermArray @elemTypes
-- proxy instance for vinyl's identity
instance ToTerm term => ToTerm (Identity term) where
   toTerm (Identity term) = toTerm term

data RESP :: Type where
  StringType :: RESP
  ErrorType :: RESP
  IntegerType :: RESP
  BulkType :: RESP
  ArrayType :: [RESP] -> RESP
  deriving Show

type FromKind :: RESP -> Type
type family FromKind kind = resp | resp -> kind where
  FromKind StringType = String
  FromKind ErrorType = Error
  FromKind IntegerType = Integer
  FromKind BulkType = BulkString
  forall (elems :: [RESP]). FromKind (ArrayType elems) = Array elems

-- helper map
type family MapFromKind (elems :: [RESP]) :: [Type] where
   MapFromKind '[] = '[]
   MapFromKind (elem : elems) = FromKind elem : MapFromKind elems

type ToKind :: Type -> RESP
-- type family ToKind resp = kind | kind -> resp where
type family ToKind resp = kind where
  ToKind String = StringType
  ToKind Error = ErrorType
  ToKind Integer = IntegerType
  ToKind BulkString = BulkType
  forall (elems :: [RESP]). ToKind (Array elems) = ArrayType elems
  -- toKind proxy instance for vinyl's identity
  ToKind (Identity term) = ToKind term

-- type HasKind :: (Type -> Type) -> Type -> Type RESP
-- type family HasKind wrapper resp = kind | kind -> resp where
--   HasKind Identity resp = ToKind resp

-- helper map
type family MapToKind (elems :: [Type]) :: [RESP] where
   MapToKind '[] = '[]
   MapToKind (elem : elems) = ToKind elem : MapToKind elems

-- | plain type-indexed labeled sum of 'RESP' types
data Term :: RESP -> Type where
  TermString :: String -> Term (ToKind String)
  TermError :: Error -> Term (ToKind Error)
  TermInteger :: Integer -> Term (ToKind Integer)
  TermBulkString :: BulkString -> Term (ToKind BulkString)
  TermArray :: forall (elemTypes :: [RESP]) (elems :: [Type])
    . (elemTypes ~ MapToKind elems, elems ~ MapFromKind elemTypes
      , Terms elems)
    => Array elemTypes -> Term (ToKind (Array elemTypes))

deriving instance forall (elem :: RESP). Show (Term elem)

data SomeTerm :: Type where
  SomeTermString :: Term (ToKind String) -> SomeTerm
  SomeTermError :: Term (ToKind Error) -> SomeTerm
  SomeTermInteger :: Term (ToKind Integer) -> SomeTerm
  SomeTermBulkString :: Term (ToKind BulkString) -> SomeTerm
  SomeTermArray :: forall (elemTypes :: [RESP]) (elems :: [Type])
    . (elemTypes ~ MapToKind elems, elems ~ MapFromKind elemTypes
      , Terms elems)
    => Term (ToKind (Array elemTypes)) -> SomeTerm

deriving instance Show SomeTerm

someTerm :: forall (term :: RESP). Term term -> SomeTerm
someTerm = \case
  term@(TermString _) -> SomeTermString term
  term@(TermError _) -> SomeTermError term
  term@(TermInteger _) -> SomeTermInteger term
  term@(TermBulkString _) -> SomeTermBulkString term
  term@(TermArray _) -> SomeTermArray term

--
-- Serializing
--

class Prefix term where
  prefix :: proxy term -> Char

instance Prefix String where prefix = const '+'
instance Prefix Error where prefix = const '-'
instance Prefix Integer where prefix = const ':'
instance Prefix BulkString where prefix = const '$'
instance Prefix Array where prefix = const '*'

-- type classes for user-defined data

class ToRESP toSerialize where
  type ToSerialize toSerialize :: RESP
  toRESP :: toSerialize -> Term (ToSerialize toSerialize)

class FromRESP toDeserialize where
  type ToDeserialize toDeserialize :: RESP
  fromRESP :: Term (ToDeserialize toDeserialize) -> FromKind toDeserialize

-- parsing helpers

null :: ByteString
null = "-1"

crlf :: ByteString
crlf = "\r\n"

crlf' :: B.ByteString
crlf' = toStrict crlf

delimiterLength :: Int
delimiterLength = B.length crlf'

-- ???: should array be able to read nil bulk string as nil array and vice versa

-- | consume \r\n
terminatedCRLF :: Monoid left
  => Either left (consumed, B.ByteString)
  -> Either left (consumed, B.ByteString)
terminatedCRLF consumed
  | Right (str, rest) <- consumed
  , B.take delimiterLength rest == crlf'
  = pure (str, B.drop delimiterLength rest)
  | otherwise = Left mempty

-- | helper unsigned natural number consumer in decimal encoding
udecimal
  :: forall (int :: Type)
  . The int '[Integral, Bounded]
  => B.ByteString
  -> Either () (int, B.ByteString)
udecimal = B.span (isDigit . w2c) >>> \case
  (want, rest)
    | B.null want -> Left ()
    | otherwise ->
      B.foldl' go (pure $ getSum mempty) want
      & maybeToRight ()
      <&> (, rest)
  where
  go :: forall (int :: Type)
    . The int '[Integral, Bounded]
    => Maybe int -> Word8 -> Maybe int
  go result (w2c -> digit) = do
    step <- result
      <&> \num ->
      fromIntegral @_ @Prelude.Integer num * base -- FIXME: /fromIntegral/toIntegralSized/
      + fromIntegral (digitToInt digit)
    guard $ step <= fromIntegral (maxBound @int)
    pure $ fromIntegral step
    where base = 10

-- | consume signed int64 in decimal encoding
decimal :: Consume ()
decimal () str = maybe (udecimal' False str) (udecimal' True)
  $ B.stripPrefix "-" str
  where
  udecimal' :: Bool -> B.ByteString -> Either () (B.ByteString, B.ByteString)
  udecimal' isNegate str
    = first (toStrict . toLazyByteString . int64Host . if isNegate then negate else id)
    <$> udecimal str

-- | consume simple byte string without intermediate \r or \n characters
simpleString :: Consume ()
simpleString () = pure . B.break (`B.elem` crlf')

-- | get simple byte string without intermediate
-- \r or \n characters, terminated by \r\n.
getLazyByteStringCRLF :: Get ByteString
getLazyByteStringCRLF
  = withInputChunks mempty ((terminatedCRLF .) . simpleString)
    fromChunks failOnEOF

-- | get signed int64 in decimal encoding
getInt64DecCRLF :: Get Int64
getInt64DecCRLF
  = withInputChunks mempty ((terminatedCRLF .) . decimal)
    (runGet getInt64host . fromChunks) failOnEOF

-- | get signed int64 in decimal encoding
getBulk :: Get (Maybe Text)
getBulk = do
  size <- getInt64DecCRLF
  maybe empty
    \case
      (-1) -> pure mempty
      bytesCount | bytesCount > 0 -> pure . decodeUtf8 <$> getByteString bytesCount
      _ -> empty
    $ toIntegralSized size

getArray :: Get SomeArray
getArray = do
  size <- getInt64DecCRLF
  maybe empty (\case
    (-1) -> pure $ SomeArray @'[] mempty
    elemNum | elemNum > 0 -> hreplicateM elemNum $ get @SomeTerm
    _ -> empty)
    $  toIntegralSized size
  where
  hreplicateM :: Applicative m => Int -> m SomeTerm -> m SomeArray
  hreplicateM times f = loop times
    where
    loop cnt
      | cnt <= 0  = pure . SomeArray $ pure RNil
      | otherwise = liftA2
        (\elem ->
          let
            cons :: forall (elemTypes :: [RESP]) (elems :: [Type])
              . (elemTypes ~ MapToKind elems, elems ~ MapFromKind elemTypes
                , Terms elems)
              => HList elems -> SomeArray
            cons = \elems' ->
              let
                add :: forall (term :: Type)
                  . (term ~ FromKind (ToKind term), ToTerm term, Show term)
                  => term -> SomeArray
                add elem' = SomeArray . pure $ Identity elem' :& elems'
              in case elem of
                SomeTermString (TermString term) -> add term
                SomeTermError (TermError term) -> add term
                SomeTermInteger (TermInteger term) -> add term
                SomeTermBulkString (TermBulkString term) -> add term
                SomeTermArray (TermArray term) -> add term
          in \case
            (SomeArray Nothing) -> cons RNil
            (SomeArray (Just elems)) -> cons elems)
        f (loop (cnt - 1))

instance Binary String where
  put string =  put (prefix $ Proxy @String)
    <> putLazyByteString (pack $ unString string)
    <> putLazyByteString crlf

  get = do
    prefix' <- get
    guard $ prefix' == prefix (Proxy @String)
    String . unpack <$> getLazyByteStringCRLF

instance Binary Error where
  put string =  put (prefix $ Proxy @Error)
    <> putLazyByteString (pack $ unError string)
    <> putLazyByteString crlf

  get = do
    prefix' <- get
    guard $ prefix' == prefix (Proxy @Error)
    Error . unpack <$> getLazyByteStringCRLF

instance Binary Integer where
  put integer = put (prefix $ Proxy @Integer)
    <>  (putBuilder . int64Dec $ unInteger integer)
    <> putLazyByteString crlf

  get = do
    prefix' <- get
    guard $ prefix' == prefix (Proxy @Integer)
    Integer <$> getInt64DecCRLF

instance Binary BulkString where
  put (unBulkString -> txt) = put (prefix $ Proxy @BulkString)
    <> maybe (putLazyByteString null)
      (\(fromStrict . encodeUtf8 -> str)
        -> (putBuilder . int64Dec) (length str)
        <> putLazyByteString crlf
        <> putLazyByteString str)
      txt
    <> putLazyByteString crlf

  get = do
    prefix' <- get
    guard $ prefix' == prefix (Proxy @BulkString)
    BulkString <$> getBulk

instance Binary SomeArray where
  put = \case
    (SomeArray Nothing) -> put (prefix $ Proxy @Array)
      <> putLazyByteString null
      <> putLazyByteString crlf
    (SomeArray (Just hlist)) -> let
      elems = reifyConstraint @ToTerm hlist & rfoldMap \case
        (Compose (Dict elem)) -> [ForAll @'[ToTerm] elem]
      in put (prefix $ Proxy @Array)
      <> (putBuilder . int64Dec . fromIntegral) (Prelude.length elems)
      <> putLazyByteString crlf
      <> mconcat (elems <&> \(ForAll elem) -> put . someTerm $ toTerm elem)

  get = do
    prefix' <- get
    guard $ prefix' == prefix (Proxy @Array)
    getArray

instance Binary SomeTerm where
  put = \case
    SomeTermString (TermString term) -> put term
    SomeTermError (TermError term) -> put term
    SomeTermInteger (TermInteger term) -> put term
    SomeTermBulkString (TermBulkString term) -> put term
    SomeTermArray (TermArray (Array term)) -> put . SomeArray $ pure term
    SomeTermArray (TermArray ArrayNull) -> put $ SomeArray @'[] mempty

  get = do
    prefix' <- lookAhead get
    if
      | prefix' == prefix (Proxy @String) -> SomeTermString . TermString
        <$> get @String
      | prefix' == prefix (Proxy @Error) -> SomeTermError . TermError
        <$> get @Error
      | prefix' == prefix (Proxy @Integer) -> SomeTermInteger . TermInteger
        <$> get @Integer
      | prefix' == prefix (Proxy @BulkString) -> SomeTermBulkString . TermBulkString
        <$> get @BulkString
      | prefix' == prefix (Proxy @Array) -> get @SomeArray <&>
        \case
          (SomeArray Nothing) -> SomeTermArray $ TermArray ArrayNull
          (SomeArray (Just hlist)) -> SomeTermArray . TermArray $ Array hlist

-- auxiliary machinery

type The :: forall typ. typ -> [typ -> Constraint] -> Constraint
type family The constraint types where
  The typ '[] = ()
  The typ (constraint : rest) = (constraint typ, The typ rest)

data ForAll (constraints :: [Type -> Constraint])
  = forall (typ :: Type). The typ constraints => ForAll typ
