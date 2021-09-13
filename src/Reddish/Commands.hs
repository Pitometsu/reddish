module Reddish.Commands where

import Prelude hiding (String, Integer)
import qualified Prelude (String, Integer)

import Reddish.RESP

import Data.List.NonEmpty (NonEmpty)

type Key = BulkString

-- newtype CommandsT m

-- instance MonadTrans CommandsT
-- instance Monad m => Commands CommandsT

class Monad m => Commands m where
  ping :: m String
  -- tx
  -- pack into tx monad???
  multi :: m String
  discard :: m String
  watch :: NonEmpty Key -> m String
  unwatch :: m String
  exec :: m SomeArray -- list of all values in tx
  --
  get :: Key -> m (Either Error BulkString)
  getdel :: Key -> m (Either Error BulkString)
  mget :: NonEmpty Key -> m SomeArray -- Array of BulkString same length
  -- simplified set
  set :: Key -> SomeTerm -> m (Either BulkString String) -- explicit type for nil of BulkString???
  type' :: Key -> m String
