{-# LANGUAGE OverloadedStrings #-}

module Proxy.HTTP ( relay
                  ) where

import Control.Monad (when)
import Data.ByteString.Base64 (encode)
import qualified Data.ByteString.Char8 as BS
import Data.List
import System.Socket
import System.Socket.Family.Inet
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream

import Types
import Proxy.Utils

relay :: Socket Inet Stream TCP -> SocketAddress Inet -> SocketAddress Inet -> ProxyAuth -> IO ()
relay c proxy dest auth =
  do s <- socket :: IO (Socket Inet Stream TCP)
     connect s proxy
     let firstWord = (\[_, a] -> a) . words
         addr = firstWord $ show $ inetAddress dest
         port = firstWord $ show $ inetPort dest
         host = BS.pack $ addr ++ ":" ++ port
         authHeader = case auth of
                        Nothing -> []
                        Just (u, p) -> ["Proxy-Authorization: Basic ", encode (BS.concat [u, ":", p]), "\r\n"]
         header = BS.concat $ ["CONNECT ", host, " HTTP/1.0\r\n"] ++ authHeader ++ ["\r\n"]
     sendAll s header mempty
     (b, r) <- recvHeader s
     when (BS.isPrefixOf "HTTP/1.0 200" b) $ do sendAll c r mempty
                                                transfer c s

recvHeader :: Socket Inet Stream TCP -> IO (BS.ByteString, BS.ByteString)
recvHeader s =
  do b <- receive s 1000 mempty
     return $ (\(a, b) -> (a, BS.drop 4 b)) $ BS.breakSubstring "\r\n\r\n" $ b
