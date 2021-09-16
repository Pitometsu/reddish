module Reddish.Driver where

import Prelude hiding (String, Integer)
import qualified Prelude (String, Integer)

import Reddish.Commands
import Reddish.RESP (type The)

import Control.Monad (unless, when)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow(throwM))
import Control.Monad.IO.Class (type MonadIO, liftIO)
import Control.Monad.Trans (type MonadTrans, lift)
import Control.Monad.Reader (type MonadReader, ReaderT(..), ask)
import Data.Binary (decode, encode)
import Data.ByteString.Lazy (type ByteString, fromStrict)
import Data.Functor ((<&>))
import Data.Maybe (maybe)
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Functor (Identity(..))
import Data.Word (Word16)
import Network.Simple.TCP (connect, recv, sendLazy)

import Network.Socket hiding (recv, connect, socket)
import Data.String (type IsString)

type Port = Word16

up :: (The monadic '[MonadIO, MonadMask], result ~ argument)
  => CommandsT argument (TCPConnection monadic) result
  -> monadic result
up run = connection host port chunkSize do
  remoteAddr <- ask <&> address
  liftIO . putStrLn $ "Connection established to " <> show remoteAddr
  command run
  where
  chunkSize = 7148126177 -- empirical max
  host = "localhost"
  port = 6379 :: Port

-- TODO: put chunkSize, send' and recv' into DriverSig's class

data Env = Env
  { socket :: Socket
  , address :: SockAddr
  , chunkSize :: Int }

newtype TCPConnection monadic result
  = TCPConnection { runTCPConnection :: ReaderT Env monadic result }
  deriving newtype (Functor, Applicative, Monad)

deriving newtype instance MonadIO monadicIO
  => MonadIO (TCPConnection monadicIO)
deriving newtype instance MonadThrow throwable
  => MonadThrow (TCPConnection throwable)
deriving newtype instance MonadCatch catchable
  => MonadCatch (TCPConnection catchable)
deriving newtype instance MonadMask mask
  => MonadMask (TCPConnection mask)
deriving newtype instance MonadTrans TCPConnection
deriving newtype instance Monad monadic
  => MonadReader Env (TCPConnection monadic)

connection :: The monadic '[MonadIO, MonadMask]
  => HostName
  -> Port
  -> Int
  -> TCPConnection monadic result
  -> monadic result
connection host port chunkSize conn = connect host (show port)
  \(socket, address) -> flip runReaderT Env{..} $ runTCPConnection conn

instance The monadic '[MonadIO, MonadMask] => Connection (TCPConnection monadic) where
  request term = do
    (connection, size) <- (,) <$> socket <*> chunkSize <$> ask
    sendLazy connection $ encode term
    maybeM
      (throwM ReddishEmptyCommandResponse)
      (pure . decode . fromStrict)
      $ recv connection size
    where
    maybeM :: Monad m => m b -> (a -> m b) -> m (Maybe a) -> m b
    maybeM n j x = maybe n j =<< x
