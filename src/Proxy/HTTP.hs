module Proxy.HTTP ( relay
                  ) where

import Control.Monad (when)
import qualified Data.ByteString.Char8 as BS
import Data.List
import System.Socket
import System.Socket.Family.Inet
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream

import Proxy.Utils

relay :: Socket Inet Stream TCP -> SocketAddress Inet -> SocketAddress Inet -> IO ()
relay c proxy dest = do s <- socket :: IO (Socket Inet Stream TCP)
                        connect s proxy
                        let firstWord = (\[_, a] -> a) . words
                            addr = firstWord $ show $ inetAddress dest
                            port = firstWord $ show $ inetPort dest
                            host = addr ++ ":" ++ port
                            header = BS.pack $ "CONNECT " ++ host ++ " HTTP/1.1\r\nHost: " ++ host ++ "\r\n\r\n"
                        sendAll s header mempty
                        b <- recvHeader s
                        let l = lines $ filter (/= '\r') $ BS.unpack b
                            status = head l
                            headers = tail l
                        when (isPrefixOf "HTTP/1.1 200" status && last headers == "") $ transfer c s

recvHeader :: Socket Inet Stream TCP -> IO BS.ByteString
recvHeader s = do b <- receive s 1000 mempty
                  if isSuffixOf "\n\n" (filter (/= '\r') $ BS.unpack b)
                    then return b
                    else fmap (BS.append b) (recvHeader s)
