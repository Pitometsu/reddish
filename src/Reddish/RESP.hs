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

-- existential wrapper for the Array
data SomeArray :: Type where
  SomeArray_ :: forall (elemTypes :: [RESPKind]) (elems :: [Type])
    . (elemTypes ~ MapToKind elems
      , elems ~ MapFromKind elemTypes
      , The elems '[RFoldMap, ReifyConstraint ToRESPTerm Identity])
    => !(Maybe (HList elems)) -> SomeArray

-- ???: is maxBound @Word or @Int64 -- the maximum array size
-- | smart constructor
pattern SomeArray
  :: ()
  => (elemTypes ~ MapToKind elems
    , elems ~ MapFromKind elemTypes
    , The elems '[RFoldMap, ReifyConstraint ToRESPTerm Identity])
  =>  Maybe (HList elems) -> SomeArray
pattern SomeArray unSomeArray = SomeArray_ unSomeArray

data Array :: [RESPKind] -> Type where
  Array_ :: forall (elemTypes :: [RESPKind]) (elems :: [Type])
    . (elemTypes ~ MapToKind elems, elems ~ MapFromKind elemTypes)
    => { unArray_ :: !(HList elems) } -> Array elemTypes
  ArrayNull_ :: Array '[]

-- ???: is maxBound @Word or @Int64 -- the maximum array size
-- | smart constructor
pattern Array :: forall (elemTypes :: [RESPKind]) (elems :: [Type])
  . (elemTypes ~ MapToKind elems, elems ~ MapFromKind elemTypes)
  =>  HList elems -> Array elemTypes
pattern Array { unArray } = Array_ unArray

-- -- | smart constructor
pattern ArrayNull :: forall (elems :: [RESPKind])
  . ()
  => (elems ~ '[])
  => Array elems
pattern ArrayNull = ArrayNull_

deriving instance forall (elems :: [RESPKind])
  . The (MapFromKind elems) '[RMap
  , ReifyConstraint Show Identity
  , RecordToList] => Show (Array elems)

class RESPPrefix term where
  prefix :: proxy term -> Char

instance RESPPrefix String where prefix = const '+'
instance RESPPrefix Error where prefix = const '-'
instance RESPPrefix Integer where prefix = const ':'
instance RESPPrefix BulkString where prefix = const '$'
instance RESPPrefix Array where prefix = const '*'

{-
type RESP :: Type -> Constraint -- constrain target type over ToRESPTerm'
type family RESP term where
  RESP String = ()
  RESP Error = ()
  RESP Integer = ()
  RESP BulkString = ()
  RESP (Array '[]) = ()
  forall (elem :: RESPKind) (elems :: [RESPKind]). RESP (Array (elem : elems))
    = (RESP (FromKind elem), RESP (Array elems))
-}

class ToRESPTerm term where
  toTerm :: term -> RESPTerm (ToKind term)
instance ToRESPTerm String where toTerm = RESPString
instance ToRESPTerm Error where toTerm = RESPError
instance ToRESPTerm Integer where toTerm = RESPInteger
instance ToRESPTerm BulkString where toTerm = RESPBulkString
instance forall (elemTypes :: [RESPKind]) (elems :: [Type])
  . (elemTypes ~ MapToKind elems
    , elems ~ MapFromKind elemTypes
    , The elems '[RFoldMap, ReifyConstraint ToRESPTerm Identity])
  => ToRESPTerm (Array elemTypes) where toTerm = RESPArray @elemTypes
-- proxy instance for vinyl's identity
instance ToRESPTerm term => ToRESPTerm (Identity term) where
   toTerm (Identity term) = toTerm term

data RESPKind :: Type where
  StringType :: RESPKind
  ErrorType :: RESPKind
  IntegerType :: RESPKind
  BulkType :: RESPKind
  ArrayType :: [RESPKind] -> RESPKind
  deriving Show

type FromKind :: RESPKind -> Type
type family FromKind kind = resp | resp -> kind where
  FromKind StringType = String
  FromKind ErrorType = Error
  FromKind IntegerType = Integer
  FromKind BulkType = BulkString
  forall (elems :: [RESPKind]). FromKind (ArrayType elems) = Array elems

-- helper map
type family MapFromKind (elems :: [RESPKind]) :: [Type] where
   MapFromKind '[] = '[]
   MapFromKind (elem : elems) = FromKind elem : MapFromKind elems

type ToKind :: Type -> RESPKind
-- type family ToKind resp = kind | kind -> resp where
type family ToKind resp = kind where
  ToKind String = StringType
  ToKind Error = ErrorType
  ToKind Integer = IntegerType
  ToKind BulkString = BulkType
  forall (elems :: [RESPKind]). ToKind (Array elems) = ArrayType elems
  -- toKind proxy instance for vinyl's identity
  ToKind (Identity term) = ToKind term

-- type HasKind :: (Type -> Type) -> Type -> Type RESPKind
-- type family HasKind wrapper resp = kind | kind -> resp where
--   HasKind Identity resp = ToKind resp

-- helper map
type family MapToKind (elems :: [Type]) :: [RESPKind] where
   MapToKind '[] = '[]
   MapToKind (elem : elems) = ToKind elem : MapToKind elems

-- | plain type-indexed labeled sum of 'RESP' types
data RESPTerm :: RESPKind -> Type where
  RESPString :: String -> RESPTerm (ToKind String)
  RESPError :: Error -> RESPTerm (ToKind Error)
  RESPInteger :: Integer -> RESPTerm (ToKind Integer)
  RESPBulkString :: BulkString -> RESPTerm (ToKind BulkString)
  RESPArray :: forall (elemTypes :: [RESPKind]) (elems :: [Type])
    . (elemTypes ~ MapToKind elems
      , elems ~ MapFromKind elemTypes
      , The elems '[RFoldMap, ReifyConstraint ToRESPTerm Identity])
    => Array elemTypes -> RESPTerm (ToKind (Array elemTypes))

data RESPTerm' :: Type where
  RESPString' :: RESPTerm (ToKind String) -> RESPTerm'
  RESPError' :: RESPTerm (ToKind Error) -> RESPTerm'
  RESPInteger' :: RESPTerm (ToKind Integer) -> RESPTerm'
  RESPBulkString' :: RESPTerm (ToKind BulkString) -> RESPTerm'
  RESPArray' :: forall (elemTypes :: [RESPKind]) (elems :: [Type])
    . (elemTypes ~ MapToKind elems
      , elems ~ MapFromKind elemTypes
      , The elems '[RFoldMap, ReifyConstraint ToRESPTerm Identity])
    => RESPTerm (ToKind (Array elemTypes)) -> RESPTerm'

someTerm :: forall (term :: RESPKind). RESPTerm term -> RESPTerm'
someTerm = \case
  term@(RESPString _) -> RESPString' term
  term@(RESPError _) -> RESPError' term
  term@(RESPInteger _) -> RESPInteger' term
  term@(RESPBulkString _) -> RESPBulkString' term
  term@(RESPArray _) -> RESPArray' term

--
-- Serializing
--

{-
class ToRESP toSerialize where
  type ToSerialize :: RESPKind
  toRESP :: toSerialize -> RESPTerm ToSerialize
class FromRESP toDeserialize where
  type ToDeserialize :: RESPKind
  fromRESP :: RESPTerm (ToKind ToDeserialize) -> toDeserialize
-}

instance Binary RESPTerm' where
  put = \case
    RESPString' (RESPString term) -> put term
    RESPError' (RESPError term) -> put term
    RESPInteger' (RESPInteger term) -> put term
    RESPBulkString' (RESPBulkString term) -> put term
    RESPArray' (RESPArray (Array term)) -> put . SomeArray $ pure term
    RESPArray' (RESPArray ArrayNull) -> put $ SomeArray @'[] mempty

  get = do
    prefix' <- lookAhead get
    if
      | prefix' == prefix (Proxy @String) -> RESPString' . RESPString <$> get @String
      | prefix' == prefix (Proxy @Error) -> RESPError' . RESPError <$> get @Error
      | prefix' == prefix (Proxy @Integer) -> RESPInteger' . RESPInteger <$> get @Integer
      | prefix' == prefix (Proxy @BulkString) -> RESPBulkString' . RESPBulkString <$> get @BulkString
      | prefix' == prefix (Proxy @Array) -> get @SomeArray <&>
        \case
          (SomeArray Nothing) -> RESPArray' $ RESPArray ArrayNull
          (SomeArray (Just hlist)) -> RESPArray' . RESPArray $ Array hlist

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
    elemNum | elemNum > 0 -> hreplicateM elemNum $ get @RESPTerm'
    _ -> empty)
    $  toIntegralSized size
  where
  hreplicateM :: Applicative m => Int -> m RESPTerm' -> m SomeArray
  hreplicateM times f = loop times
    where
    loop cnt
      | cnt <= 0  = pure . SomeArray $ pure RNil
      | otherwise = liftA2
        (\elem -> \case
          (SomeArray Nothing) ->
            let
              add :: forall (term :: Type). (term ~ FromKind (ToKind term), ToRESPTerm term)
                => term -> SomeArray
              add elem' = SomeArray . pure $ Identity elem' :& RNil
            in case elem of
              RESPString' (RESPString term) -> add term
              RESPError' (RESPError term) -> add term
              RESPInteger' (RESPInteger term) -> add term
              RESPBulkString' (RESPBulkString term) -> add term
              RESPArray' (RESPArray term) -> add term
          (SomeArray (Just elems)) ->
            let
              add :: forall (term :: Type). (term ~ FromKind (ToKind term), ToRESPTerm term)
                => term -> SomeArray
              add elem' = SomeArray . pure $ Identity elem' :& elems
            in case elem of
              RESPString' (RESPString term) -> add term
              RESPError' (RESPError term) -> add term
              RESPInteger' (RESPInteger term) -> add term
              RESPBulkString' (RESPBulkString term) -> add term
              RESPArray' (RESPArray term) -> add term)
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
instance Binary SomeArray
  where
  put = \case
    (SomeArray Nothing) -> put (prefix $ Proxy @Array)
      <> putLazyByteString null
      <> putLazyByteString crlf
    (SomeArray (Just hlist)) -> let
      elems = reifyConstraint @ToRESPTerm hlist & rfoldMap \case
        (Compose (Dict elem)) -> [ForAll @'[ToRESPTerm] elem]
      in put (prefix $ Proxy @Array)
      <> (putBuilder . int64Dec . fromIntegral) (Prelude.length elems)
      <> putLazyByteString crlf
      <> mconcat (elems <&> \(ForAll elem) -> put . someTerm $ toTerm elem)
  get = do
    prefix' <- get
    guard $ prefix' == prefix (Proxy @Array)
    getArray

type The :: forall typ. typ -> [typ -> Constraint] -> Constraint
type family The constraint types where
  The typ '[] = ()
  The typ (constraint : rest) = (constraint typ, The typ rest)

data ForAll (constraints :: [Type -> Constraint])
  = forall (typ :: Type). The typ constraints => ForAll typ
