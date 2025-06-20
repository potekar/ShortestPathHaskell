{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
import Web.Scotty
import Network.Wai.Middleware.Cors
import Data.Aeson (FromJSON, parseJSON, withObject, (.:), encode, decode, ToJSON, object, (.=))
import qualified Data.ByteString.Lazy as BL
import Dijkstra
import qualified Data.Map as Map
import Network.HTTP.Types.Status (status400)
import GHC.Generics (Generic)
import Network.Wai (Middleware, Request, pathInfo, requestMethod)
import Control.Monad.IO.Class (liftIO)

-- Types matching Elm's JSON structure

data Edge = Edge { to :: Int, weight :: Int } deriving (Show, Generic)
instance FromJSON Edge

data Node = Node { node :: Int, edges :: [Edge] } deriving (Show, Generic)
instance FromJSON Node

-- DijkstraRequest now uses [Node] instead of Graph
data DijkstraRequest = DijkstraRequest
  { graph :: [Node]
  , start :: Int
  , end :: Int
  } deriving (Show, Generic)

instance FromJSON DijkstraRequest where
  parseJSON = withObject "DijkstraRequest" $ \v ->
    DijkstraRequest <$> v .: "graph" <*> v .: "start" <*> v .: "end"

-- Convert [Node] to Graph (Map Int [(Int, Int)])
nodeListToGraph :: [Node] -> Graph
nodeListToGraph = Map.fromList . map (\n -> (node n, map (\e -> (to e, weight e)) (edges n)))

-- Logging middleware
logRequests :: Middleware
logRequests app req respond = do
    putStrLn $ "Request: " ++ show (requestMethod req) ++ " " ++ show (pathInfo req)
    app req respond

instance ToJSON DijkstraStep

main :: IO ()
main = scotty 3000 $ do
    middleware logRequests
    -- Enable CORS
    middleware $ cors $ const $ Just simpleCorsResourcePolicy
        { corsRequestHeaders = ["Content-Type"]
        , corsMethods = ["GET", "POST"]
        }

    -- Serve static files
    get "/" $ file "elm/index.html"
    get "/elm.js" $ file "elm/elm.js"
    get "/style.css" $ file "elm/style.css"

    -- API endpoint for Dijkstra's algorithm
    post "/api/shortest-path" $ do
        body <- body
        case decode body of
            Just (req :: DijkstraRequest) -> do
                let g = nodeListToGraph (graph req)
                let s = start req
                let e = end req
                let (distances, previous, steps) = dijkstraWithSteps g s (Just e)
                let shortestPathNodes = getShortestPath previous s e
                json $ object [ "distances" .= Map.toList distances, "steps" .= steps, "shortestPath" .= shortestPathNodes ]
            Nothing -> status status400 >> text "Invalid graph format" 