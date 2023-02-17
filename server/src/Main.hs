{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.List (intersperse)
import Data.Pool
import Data.Text (Text)
import Data.Text qualified as Text
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.Types (PGArray (..), fromPGArray)
import Lucid
import Lucid.Servant (safeAbsHref_)
import Network.Wai.Handler.Warp qualified as Warp
import Options.Applicative
import Servant
import Servant.HTML.Lucid

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

type App =
  "api" :> Api
    :<|> "node" :> Capture "node_id" Int :> Get '[HTML] (Document (InOut Node))
    :<|> "nodelabel" :> Capture "label" Text :> Get '[HTML] (Document (InOut [Node]))
    :<|> Raw

type Api =
  "graph" :> Get '[JSON] [Node]
    :<|> "node" :> Capture "node_id" Int :> Get '[JSON] Node
    :<|> "edge" :> Capture "edge_id" Int :> Get '[JSON] Edge

data Node = Node
  { nodeId :: Int
  , nodeLabels :: [Text]
  , nodeProperties :: Value
  , nodeIn :: [Edge]
  , nodeOut :: [Edge]
  }

data Edge = Edge
  { edgeId :: Int
  , edgeLabels :: [Text]
  , edgeProperties :: Value
  , edgeA :: Int
  , edgeB :: Int
  }

instance ToJSON Node where
  toJSON node =
    object
      [ "node_id" .= nodeId node
      , "labels" .= nodeLabels node
      , "properties" .= nodeProperties node
      , "in" .= nodeIn node
      , "out" .= nodeOut node
      ]

instance ToJSON Edge where
  toJSON edge =
    object
      [ "edge_id" .= edgeId edge
      , "labels" .= edgeLabels edge
      , "properties" .= edgeProperties edge
      , "a" .= edgeA edge
      , "b" .= edgeB edge
      ]

id' :: Monad m => ToHtml a => a -> HtmlT m ()
id' a = div_ [class_ "id"] (toHtml a)

labels :: Monad m => ToHtml a => a -> HtmlT m ()
labels a = div_ [class_ "labels"] (toHtml a)

label :: Monad m => ToHtml a => a -> HtmlT m ()
label a = div_ [class_ "label"] (toHtml a)

nodeLink :: Monad m => Int -> Text -> HtmlT m ()
nodeLink nodeId txt = a_ [safeAbsHref_ (Proxy :: Proxy App) (Proxy :: Proxy ("node" :> Capture "node_id" Int :> Get '[HTML] (Document (InOut Node)))) nodeId] (toHtml txt)

nodeLabelLink :: Monad m => Text -> Text -> HtmlT m ()
nodeLabelLink lbl txt = a_ [safeAbsHref_ (Proxy :: Proxy App) (Proxy :: Proxy ("nodelabel" :> Capture "label" Text :> Get '[HTML] (Document (InOut [Node])))) lbl] (toHtml txt)

newtype Properties = Properties Value

instance ToHtml Properties where
  toHtml (Properties xs) = div_ [class_ "properties"] (toHtml xs)
  toHtmlRaw = toHtml

instance ToHtml Value where
  toHtml (String x) = div_ [classes_ ["value", "string"]] (toHtml x)
  toHtml (Number x) = div_ [classes_ ["value", "number"]] (toHtml $ show x)
  toHtml (Bool x) = div_ [classes_ ["value", "bool"]] (if x then "True" else "False")
  toHtml Null = div_ [classes_ ["value", "null"]] "Null"
  toHtml (Array xs) = div_ [classes_ ["value", "array"]] (mapM_ toHtml xs)
  toHtml (Object xs) = div_ [classes_ ["value", "object"]] (KeyMap.foldMapWithKey property xs)
  toHtmlRaw = toHtml

property :: Monad m => Key.Key -> Value -> HtmlT m ()
property key val = do
  div_ [class_ "key"] (toHtml $ Key.toText key)
  div_ [class_ "val"] (toHtml val)

instance ToHtml Node where
  toHtml node = do
    div_ [class_ "edges in"] $ do
      forM_ (nodeIn node) $ \edge -> do
        div_ [class_ "edge"] $ do
          id' $ nodeLink (edgeA edge) (Text.pack $ show $ edgeA edge)
          labels (mapM_ (label . toHtml) $ edgeLabels edge)
    div_ [class_ "nodes"] $ do
      div_ [class_ "node"] $ do
        id' $ Text.pack $ show $ nodeId node
        labels (mapM_ (\lbl -> label $ nodeLabelLink lbl lbl) $ nodeLabels node)
        toHtml $ Properties $ nodeProperties node
    div_ [class_ "edges out"] $ do
      forM_ (nodeOut node) $ \edge -> do
        div_ [class_ "edge"] $ do
          id' $ nodeLink (edgeB edge) (Text.pack $ show $ edgeB edge)
          labels (mapM_ (label . toHtml) $ edgeLabels edge)

  toHtmlRaw = toHtml

instance ToHtml [Node] where
  toHtml nodes = mconcat (intersperse (div_ [class_ "header"] mempty) (map toHtml nodes))
  toHtmlRaw = toHtml

newtype Document a = Document a

instance ToHtml a => ToHtml (Document a) where
  toHtml (Document a) = do
    doctype_
    html_ [lang_ "en-US"] $ do
      head_ $ do
        title_ "Graph View"
        link_ [href_ "/css/style.css", rel_ "stylesheet"]
        link_ [href_ "https://fonts.googleapis.com/css?family=Roboto Condensed", rel_ "stylesheet"]
        meta_ [charset_ "utf-8"]
        meta_ [name_ "viewport", content_ "width=device-width"]
      body_ $ toHtml a
  toHtmlRaw = toHtml

newtype InOut a = InOut a

instance ToHtml a => ToHtml (InOut a) where
  toHtml (InOut a) = div_ [class_ "in-node-out"] $ toHtml a
  toHtmlRaw = toHtml

app :: Env -> Server App
app env =
  api env
    :<|> fmap (Document . InOut) . getNode env
    :<|> fmap (Document . InOut) . getNodesLabelled env
    :<|> serveDirectoryFileServer (webroot $ envOptions env)

api :: Env -> Server Api
api env = getGraph env :<|> getNode env :<|> getEdge env

single :: [a] -> Handler a
single [] = throwError err404
single [x] = return x
single _ = throwError err500

getGraph :: Env -> Handler [Node]
getGraph env = withResource (envDbPool env) $ \conn -> do
  nodes <- liftIO (query_ conn "select id, labels, properties from nodes")
  forM nodes $ \node@(nodeId, _, _) -> do
    ins <- liftIO (query conn "select id, labels, properties, a, b from edges where b=?" (Only nodeId))
    outs <- liftIO (query conn "select id, labels, properties, a, b from edges where a=?" (Only nodeId))
    return (mkNode node ins outs)
 where
  mkNode (nid, lbls, props) i o = Node nid (fromPGArray lbls) props (map mkEdge i) (map mkEdge o)
  mkEdge (eid, lbls, props, a, b) = Edge eid (fromPGArray lbls) props a b

getNode :: Env -> Int -> Handler Node
getNode env nodeId = withResource (envDbPool env) $ \conn -> do
  node <- single =<< liftIO (query conn "select id, labels, properties from nodes where id=?" (Only nodeId))
  ins <- liftIO (query conn "select id, labels, properties, a, b from edges where b=?" (Only nodeId))
  outs <- liftIO (query conn "select id, labels, properties, a, b from edges where a=?" (Only nodeId))

  return (mkNode node ins outs)
 where
  mkNode (nid, lbls, props) i o = Node nid (fromPGArray lbls) props (map mkEdge i) (map mkEdge o)
  mkEdge (eid, lbls, props, a, b) = Edge eid (fromPGArray lbls) props a b

getEdge :: Env -> Int -> Handler Edge
getEdge env edgeId = withResource (envDbPool env) $ \conn -> do
  rows <- liftIO $ query conn "select id, labels, properties, a, b from edges where id=?" (Only edgeId)
  case rows of
    [] -> throwError $ err404{errBody = "Edge not found"}
    [edge] -> return (mkEdge edge)
    _ -> throwError $ err500{errBody = "Multiple edges for id"}
 where
  mkEdge (eid, lbls, props, a, b) = Edge eid (fromPGArray lbls) props a b

getNodesLabelled :: Env -> Text -> Handler [Node]
getNodesLabelled env label = withResource (envDbPool env) $ \conn -> do
  nodes <- liftIO (query conn "select id, labels, properties from nodes where labels @> ?" (Only $ PGArray [label]))
  forM nodes $ \node@(nodeId, _, _) -> do
    ins <- liftIO (query conn "select id, labels, properties, a, b from edges where b=?" (Only nodeId))
    outs <- liftIO (query conn "select id, labels, properties, a, b from edges where a=?" (Only nodeId))
    return (mkNode node ins outs)
 where
  mkNode (nid, lbls, props) i o = Node nid (fromPGArray lbls) props (map mkEdge i) (map mkEdge o)
  mkEdge (eid, lbls, props, a, b) = Edge eid (fromPGArray lbls) props a b

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