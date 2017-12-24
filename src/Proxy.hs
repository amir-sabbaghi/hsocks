module Proxy ( handleConnection
             ) where

import Data.ByteString.Char8 (pack)
import System.Socket
import System.Socket.Family.Inet
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream
import System.Socket.Unsafe

import Types
import Proxy.Socks as S
import Proxy.HTTP as H

handleConnection :: Socket Inet Stream TCP -> ProxyType -> String -> InetPort -> ProxyAuth -> IO ()
handleConnection s t ph pp auth =
  do addr <- unsafeGetSocketOption s 0 80
     proxyAddr <- getAddressInfo (Just $ pack ph) Nothing mempty :: IO [AddressInfo Inet Stream TCP]
     let proxy = (socketAddress (head proxyAddr)) { inetPort = pp }
     case t of
       SocksProxy -> S.relay s proxy addr
       HTTPProxy  -> H.relay s proxy addr auth
