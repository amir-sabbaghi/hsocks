module Proxy.Utils where

import Control.Concurrent.Async
import Control.Monad (when)
import Data.ByteString as BS
import System.Socket
import System.Socket.Type.Stream

transfer :: Socket f Stream p -> Socket f Stream p -> IO ()
transfer c s = do
  a <- async $ forward c s
  b <- async $ forward s c
  waitAnyCancel [a, b]
  return ()

forward :: Socket f Stream p -> Socket f Stream p -> IO ()
forward r s = do
  b <- receive r (1024 * 1024) mempty
  when (not $ BS.null b) $ do
    sendAll s b mempty
    forward r s
