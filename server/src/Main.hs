{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), Value, object, (.=))
import Data.Pool
import Data.Text (Text)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (fromPGArray)
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative
import Servant

-- Command line options and parser

data Options = Options
  { webroot :: FilePath
  , port :: Warp.Port
  , dbname :: String
  , dbhost :: String
  , dbport :: Int
  , dbuser :: String
  , dbpass :: String
  }
  deriving (Show)

options :: Parser Options
options =
  Options
    <$> strOption (long "webroot" <> metavar "PATH" <> help "Serve files from the webroot")
    <*> option auto (long "port" <> metavar "PORT" <> value 8080 <> help "Port to listen on" <> showDefault)
    <*> strOption (long "dbname" <> metavar "DATABASE" <> value "discovery" <> help "Database name" <> showDefault)
    <*> strOption (long "dbhost" <> metavar "HOSTNAME" <> value "localhost" <> help "Database host to connect to" <> showDefault)
    <*> option auto (long "dbport" <> metavar "PORT" <> value 5432 <> help "Database port to connect to" <> showDefault)
    <*> strOption (long "dbuser" <> metavar "USERNAME" <> value "" <> help "Database user to authenticate with")
    <*> strOption (long "dbpass" <> metavar "PASSWORD" <> value "" <> help "Database password to authenticate with")

-- Database

type DbPool = Pool Connection

withDbPool :: ConnectInfo -> (DbPool -> IO a) -> IO a
withDbPool conninfo a = do
  pool <- createPool (connect conninfo) close 1 5 4
  result <- a pool
  destroyAllResources pool
  return result

-- Environment

data Env = Env
  { envOptions :: Options
  , envDbPool :: DbPool
  }
  deriving (Show)

-- App

type App = "api" :> Api :<|> Raw

type Api =
  "node" :> Capture "node_id" Int :> Get '[JSON] Node
    :<|> "edge" :> Capture "edge_id" Int :> Get '[JSON] Edge

data Node = Node
  { nodeLabels :: [Text]
  , nodeProperties :: Value
  , nodeIn :: [Edge]
  , nodeOut :: [Edge]
  }

data Edge = Edge
  { edgeLabels :: [Text]
  , edgeProperties :: Value
  , edgeA :: Int
  , edgeB :: Int
  }

instance ToJSON Node where
  toJSON node =
    object
      [ "labels" .= nodeLabels node
      , "properties" .= nodeProperties node
      , "in" .= nodeIn node
      , "out" .= nodeOut node
      ]

instance ToJSON Edge where
  toJSON edge =
    object
      [ "labels" .= edgeLabels edge
      , "properties" .= edgeProperties edge
      , "a" .= edgeA edge
      , "b" .= edgeB edge
      ]

app :: Env -> Server App
app env = api env :<|> serveDirectoryFileServer (webroot $ envOptions env)

api :: Env -> Server Api
api env = getNode env :<|> getEdge env

single :: [a] -> Handler a
single [] = throwError err404
single [x] = return x
single _ = throwError err500

getNode :: Env -> Int -> Handler Node
getNode env nodeId = withResource (envDbPool env) $ \conn -> do
  node <- single =<< liftIO (query conn "select labels, properties from nodes where id=?" (Only nodeId))
  ins <- liftIO (query conn "select labels, properties, a, b from edges where b=?" (Only nodeId))
  outs <- liftIO (query conn "select labels, properties, a, b from edges where a=?" (Only nodeId))

  return (mkNode node ins outs)
 where
  mkNode (lbls, props) i o = Node (fromPGArray lbls) props (map mkEdge i) (map mkEdge o)
  mkEdge (lbls, props, a, b) = Edge (fromPGArray lbls) props a b

getEdge :: Env -> Int -> Handler Edge
getEdge env edgeId = withResource (envDbPool env) $ \conn -> do
  rows <- liftIO $ query conn "select labels, properties, a, b from edges where id=?" (Only edgeId)
  case rows of
    [] -> throwError $ err404{errBody = "Edge not found"}
    [edge] -> return (mkEdge edge)
    _ -> throwError $ err500{errBody = "Multiple edges for id"}
 where
  mkEdge (lbls, props, a, b) = Edge (fromPGArray lbls) props a b

-- Main

main :: IO ()
main = execParser parser >>= server
 where
  parser =
    info
      (options <**> helper)
      (fullDesc <> progDesc "Serve graph-view app and api" <> header "graph-view-server - web server")

server :: Options -> IO ()
server opts = do
  let conninfo = ConnectInfo (dbhost opts) (fromIntegral $ dbport opts) (dbuser opts) (dbpass opts) (dbname opts)
  withDbPool conninfo $ \pool -> do
    let env = Env opts pool
    Warp.run 8080 (serve (Proxy @App) (app env))