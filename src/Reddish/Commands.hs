module Reddish.Commands where

import Prelude hiding (String, Integer)
import qualified Prelude (String, Integer)

import Reddish.RESP

import Control.Exception (type Exception)
import Control.Monad.Cont
import Control.Monad.Catch (type MonadMask, MonadThrow(throwM))
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Vinyl.Core (pattern (:&), pattern RNil)
import Data.Vinyl.Functor (pattern Identity)
import GHC.Generics (Generic)

type Key = BulkString

type CommandsT :: Type -> (Type -> Type) -> Type -> Type
newtype CommandsT result monadic argument
  = CommandsT { runCommandsT :: ContT result monadic argument }
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadIO monadicIO
  => MonadIO (CommandsT result monadicIO)
deriving newtype instance MonadThrow throwable
  => MonadThrow (CommandsT result throwable)
deriving newtype instance MonadTrans (CommandsT result)

command :: (Monad monadic, result ~ argument)
  => CommandsT result monadic argument -> monadic result
command = flip runContT pure . runCommandsT

data ReddishException
  = ReddishUnexpectedCommandResponse
    { unReddishUnexpectedCommandResponse :: Prelude.String }
  | ReddishEmptyCommandResponse
  deriving (Show, Generic)

instance Exception ReddishException

class Monad connection => Connection connection where
  request :: SomeTerm -> connection SomeTerm

instance The conn '[Connection, MonadThrow]
  => Commands (CommandsT result conn) where

  ping = do
    response <- lift $ request msg
    case response of
      (SomeTermString pong@(TermString _)) -> pure pong
      malformedTerm -> throwM . ReddishUnexpectedCommandResponse
        $ "Expected a String term, but received: "
        <> show malformedTerm
    where
    msg = SomeTermArray . TermArray . Array
      $ Identity (BulkString $ Just "PING") :& RNil

class Monad cmd => Commands cmd where
  ping :: cmd (Term StringType)
{-
  -- tx
  -- pack into tx monad???
  multi :: m String
  discard :: m String
  watch :: NonEmpty Key -> m String
  unwatch :: m String
  exec :: m SomeArray -- list of all values in tx
  --or BulkString)
  mget :: NonEmpty Key -> m SomeArray -- Array of BulkString same length
  -- simplified set
  set :: Key -> SomeTerm -> m (Either BulkString String) -- explicit type for nil of BulkString???
  type' :: Key -> m String
-}
