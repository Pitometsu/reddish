module Reddish.Driver where

import Reddish.Commands
import Reddish.RESP

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (type MonadIO, liftIO)
import Data.Binary (encode)
import Data.Maybe (maybe)
import Data.Vinyl.Core (Rec(..))
import Data.Vinyl.Functor (Identity(..))
import Data.Word (Word16)
import Network.Simple.TCP (connect, recv, sendLazy)

-- up :: (The m '[MonadIO, MonadMask], Commands c)
--   => c result
--   -> m ()
up :: The m '[MonadIO, MonadMask]
  => m ()
up = connect host (show port) \(connectionSocket, remoteAddr) -> do
  liftIO . putStrLn $ "Connection established to " <> show remoteAddr
  let msg = encode . SomeTermArray . TermArray . Array $ Identity (BulkString $ Just "PING") :& RNil
  liftIO . print $ " >> " <> msg
  sendLazy connectionSocket $ msg
  response <- recv connectionSocket chunkSize
  liftIO . print $ maybe "There's no Redis response =(" (" << " <>) response
  pure mempty
  where
    chunkSize = 7148126177 -- empirical max
    host = "localhost"
    port = 6379 :: Word16
