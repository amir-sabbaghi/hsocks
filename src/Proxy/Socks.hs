module Proxy.Socks
  ( relay
  ) where

import Control.Concurrent.Async
import Data.ByteString
import Data.Word
import System.Socket
import System.Socket.Family.Inet
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream

import Proxy.Utils

relay :: Socket Inet Stream TCP -> SocketAddress Inet -> SocketAddress Inet -> IO ()
relay c proxy dest = do
  s <- socket :: IO (Socket Inet Stream TCP)
  connect s proxy
  let (a1, a2, a3, a4) = inetAddressToTuple $ inetAddress dest
      port = read . (\[_, a] -> a) . words . show $ inetPort dest
      (p1, p2) = port `divMod` 0x100
      header = pack $ [4, 1, fromInteger p1, fromInteger p2, a1, a2, a3, a4, 0]
  sendAll s header mempty
  b <- receive s 8 mempty
  let [0, status, _, _, _, _, _, _] = unpack b
  if status == 0x5A
    then transfer c s
    else return ()
