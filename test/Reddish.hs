module Reddish where

import "reddish" Reddish

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)

test :: IO ()
test = void $ up do
  ((\case TermString (String payload) -> payload) -> pong) <- ping
  liftIO . print $ ">> " <> pong
  pure pong
-- do
--   pong <- ping
--   let name = BulkString "Name"
--   result <- set name (SomeTermString . TermString $ String "John Doe")
--   name' <- get name
--   typ <- type' name
--   pure (typ, name')
