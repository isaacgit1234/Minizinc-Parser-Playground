{-# LANGUAGE OverloadedStrings #-}

-- | Main module for launching the web server that handles parsing and
-- solving MiniZinc-lite models. This server exposes an API that accepts a
-- model, parses and solves it, and returns the resulting assignment.
module Main (main) where

import Assignment (miniZincParser, solveProblem, formatSolution, getTime)
import Control.Arrow (second)
import Control.Concurrent.QSemN (newQSemN)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Aeson.Key (fromString)
import Data.Text.Lazy (Text, pack, unpack)
import Instances (ParseResult (..), parse)
import Web.Scotty (ActionM, formParam, json, post, scotty)

-- Magic code to convert key, value pairs to JSON to send back to the server
jsonResponse :: [(String, String)] -> ActionM ()
jsonResponse pairs =
    json $
        object
            [ fromString key .= (pack value :: Text) | (key, value) <- pairs
            ]

-- --------------------------------------------------------------------------
-- | Parses and solves a posted model. Left holds a parse error; Right
-- holds the solver's answer (Nothing when there is no solution).
-- Shared by both routes so the parse-then-solve steps live in one place.
parseAndSolve :: Text -> Either String (Maybe [(String, Int)])
parseAndSolve modelText = case parse miniZincParser (unpack modelText) of
    Error err -> Left (show err)
    Result _ model -> Right (solveProblem model)

-- | Entry point – launches Scotty web server and defines API routes
main :: IO ()
main =
    newQSemN 1 >>= \_ -> scotty 3000 $ do
        post "/api/minizinc" $ do
            modelText <- formParam "model" :: ActionM Text
            liftIO $ putStrLn (unpack modelText)
            case parseAndSolve modelText of
                Left err -> jsonResponse [("error", err)]
                Right Nothing -> jsonResponse [("result", "No solution")]
                Right (Just env) -> jsonResponse (map (second show) env)

        -- Re-parses and re-solves the submitted model, then writes the
        -- solution to a timestamped file on disk, per the Part C spec.
        post "/api/save" $ do
            modelText <- formParam "model" :: ActionM Text
            case parseAndSolve modelText of
                Left err -> jsonResponse [("error", err)]
                Right Nothing -> jsonResponse [("error", "No solution")]
                Right (Just env) -> do
                    time <- liftIO getTime
                    let filename = "solution_" ++ time ++ ".out"
                    liftIO (writeFile filename (formatSolution time env))
                    jsonResponse [("saved", filename)]