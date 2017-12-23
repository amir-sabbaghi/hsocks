module Types where

import Text.Read

data ProxyType = SocksProxy

instance Read ProxyType where
  readPrec = parens (do Ident s <- lexP
                        case s of
                          "socks" -> return SocksProxy
                          _       -> pfail
                    )
