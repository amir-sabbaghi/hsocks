module Proxy ( handleConnection
             ) where

import System.Socket
import System.Socket.Family.Inet
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream
import System.Socket.Unsafe

import Types
import Proxy.Socks as S

handleConnection :: Socket Inet Stream TCP -> ProxyType -> SocketAddress Inet -> IO ()
handleConnection s SocksProxy proxy = do addr <- unsafeGetSocketOption s 0 80
                                         S.relay s proxy addr
