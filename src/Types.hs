module Types where

import Data.ByteString
import Text.Read

data ProxyType = SocksProxy
               | HTTPProxy

instance Read ProxyType where
  readPrec = parens (do Ident s <- lexP
                        case s of
                          "socks" -> return SocksProxy
                          "http"  -> return HTTPProxy
                          _       -> pfail
                    )

type ProxyAuth = Maybe (ByteString, ByteString)
