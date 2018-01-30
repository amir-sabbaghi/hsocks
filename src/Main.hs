import Control.Concurrent
import Control.Exception (finally)
import Control.Monad (forever)
import System.Socket
import System.Socket.Family.Inet
import System.Socket.Protocol.TCP
import System.Socket.Type.Stream

import Data.List.Split
import Data.Monoid
import Options.Applicative
import System.IO (stderr)
import System.Log.Formatter (simpleLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (streamHandler)
import System.Log.Logger

import Proxy
import Types

data Flags = Flags
  { flagListenHost :: String
  , flagListenPort :: Integer
  , flagProxyType :: String
  , flagProxyHost :: String
  , flagProxyPort :: Integer
  , flagProxyAuth :: ProxyAuth
  }

parseFlags :: Parser Flags
parseFlags =
  Flags <$> strOption (short 'h' <> long "host" <> metavar "Host addres to listen to" <> help "The host address to listen for incoming connections") <*>
  option auto (short 'l' <> long "port" <> metavar "Port number to listen to" <> help "The port number to listen for incoming connections") <*>
  strOption (short 't' <> long "type" <> metavar "Type of proxy" <> help "The type of proxy to use") <*>
  strOption (short 'x' <> long "proxyhost" <> metavar "Proxy address" <> help "The host to use for proxy connections") <*>
  option auto (short 'p' <> long "proxyport" <> metavar "Proxy port number" <> help "The port number to use for proxy connections") <*>
  optional
    ((,) <$> strOption (short 'u' <> long "username" <> metavar "Username for proxy server" <> help "The username used for authenticating with the proxy server") <*>
     strOption (long "password" <> metavar "Password for proxy server" <> help "The password used for authenticating with the proxy server"))

parseHost :: String -> InetAddress
parseHost h = inetAddressFromTuple $ (\[a, b, c, d] -> (a, b, c, d)) $ map read $ splitOn "." h

main :: IO ()
main = do
  h <- streamHandler stderr DEBUG
  let h' = setFormatter h $ simpleLogFormatter "[\ESC[33m$prio\ESC[0m][\ESC[1m\ESC[96m$loggername\ESC[0m] $msg"
  updateGlobalLogger rootLoggerName $ (setHandlers [h'] . setLevel DEBUG)
  (Flags host port pt ph pp auth) <- execParser opts
  let addr = parseHost host
      t = read pt
  server (SocketAddressInet addr $ fromInteger port) t ph (fromInteger pp) auth
  where
    opts = info (parseFlags <**> helper) (fullDesc <> progDesc "This is hsocks server" <> header "hsocks - a plain to transparent proxy middleware")

server :: SocketAddress Inet -> ProxyType -> String -> InetPort -> ProxyAuth -> IO ()
server l t ph pp auth = do
  s <- socket :: IO (Socket Inet Stream TCP)
  setSocketOption s (ReuseAddress True)
  bind s l
  listen s 100
  infoM "Server" $ "Listening on " ++ show (inetPort l)
  forever $ do
    (s, r) <- accept s
    infoM "Server" $ "New connection from " ++ show r
    forkIO $ finally (handleConnection s t ph pp auth) (close s)
