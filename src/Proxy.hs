module Proxy ( handleConnection
             ) where

import System.Socket
import System.Socket.Family.Inet
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream
import System.Socket.Unsafe

import Types
import Proxy.Socks as S
import Proxy.HTTP as H

handleConnection :: Socket Inet Stream TCP -> ProxyType -> SocketAddress Inet -> IO ()
handleConnection s t proxy = do addr <- unsafeGetSocketOption s 0 80
                                case t of
                                  SocksProxy -> S.relay s proxy addr
                                  HTTPProxy  -> H.relay s proxy addr
