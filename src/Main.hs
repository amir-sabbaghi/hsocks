import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (forever)
import System.Socket
import System.Socket.Family.Inet
import System.Socket.Type.Stream
import System.Socket.Protocol.TCP

import Data.Monoid
import Data.List.Split
import Options.Applicative

import Proxy
import Types

data Flags = Flags
    { flagListenHost :: String
    , flagListenPort :: Integer
    , flagProxyType :: String
    , flagProxyHost  :: String
    , flagProxyPort :: Integer
    }

parseFlags :: Parser Flags
parseFlags = Flags
        <$> strOption (short 'h' <> long "host" <> metavar "Host addres to listen to" <> help "The host address to listen for incoming connections")
        <*> option auto (short 'l' <> long "port" <> metavar "Port number to listen to" <> help "The port number to listen for incoming connections")
        <*> strOption (short 't' <> long "type" <> metavar "Type of proxy" <> help "The type of proxy to use")
        <*> strOption (short 'x' <> long "proxyhost" <> metavar "Proxy address" <> help "The host to use for proxy connections")
        <*> option auto (short 'p' <> long "proxyport" <> metavar "Proxy port number" <> help "The port number to use for proxy connections")

parseHost :: String -> InetAddress
parseHost h = inetAddressFromTuple $ (\[a, b, c, d] -> (a, b, c, d)) $ map read $ splitOn "." h

main :: IO ()
main = do (Flags host port pt ph pp) <- execParser opts
          let addr = parseHost host
              t = read pt
          server (SocketAddressInet addr $ fromInteger port) t ph (fromInteger pp)
            where opts = info (parseFlags <**> helper)
                              (fullDesc <> progDesc "This is hsocks server"
                                        <> header "hsocks - a plain to transparent proxy middleware")


server :: SocketAddress Inet -> ProxyType -> String -> InetPort -> IO ()
server l t ph pp = do s <- socket :: IO (Socket Inet Stream TCP)
                      setSocketOption s (ReuseAddress True)
                      bind s l
                      listen s 100
                      forever $ do (s, _) <- accept s
                                   forkIO $ finally (handleConnection s t ph pp) (close s)
