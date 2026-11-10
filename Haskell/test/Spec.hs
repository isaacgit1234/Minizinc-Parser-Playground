{-# LANGUAGE TypeApplications #-}

import Assignment
import Control.Exception (IOException, try)
import Control.Monad (forM)
import Data.Char (isSpace)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)

import Instances (ParseResult (Error, Result), parse)
import System.Directory (createDirectoryIfMissing)
import System.FilePath
    ( joinPath
    , replaceExtension
    , splitDirectories
    , splitFileName
    , takeBaseName
    , takeFileName
    , (</>)
    )
import System.FilePath.Glob (glob)
import System.IO.Error (isDoesNotExistError)
import Text.Read (readMaybe)

-- | A result from 'solveProblem': either a single solution or 'Nothing'.
type Solution = Maybe [(String, Int)]

-- | Normalise the order of bindings (alphabetical by variable name)
normalise :: Solution -> Solution
normalise = fmap (sortOn fst)

-- | Parse one line of an expected output file. Each line is either the word
-- @Nothing@ (no solution) or a shown binding list such as
-- @[("x",2),("y",1)]@.
readExpected :: String -> Maybe Solution
readExpected line
    | trim line == "Nothing" = Just Nothing
    | otherwise = Just <$> readMaybe (trim line)
  where
    trim :: String -> String
    trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse

-- | Does @solution@ match any of the accepted answers?
isAccepted :: [String] -> Solution -> Bool
isAccepted expectedLines solution =
    any (== normalise solution) accepted
  where
    accepted = mapMaybe (fmap normalise . readExpected) expectedLines

-- Example Model (Parser and Solver)
exampleModel :: ADT
exampleModel =
    ADT
        { declarations =
            [VariableDeclaration "x" (Range (IntLit 1) (IntLit 3))]
        , constraints = [Compare OpEq (Var "x") (IntLit 2)]
        , solverGoal = Satisfy
        }

checkADTExampleModel :: IO ()
checkADTExampleModel = do
    let model = "var 1..3: x; constraint x = 2; solve satisfy;"
    putStrLn "Testing ADT's Show instance:"
    putStrLn $ "Input string: " ++ model
    putStrLn $ case parse miniZincParser model of
        Error _ -> "Parse error"
        Result _ adt -> "Result: " ++ show adt ++ "\n" ++ "Eq: " ++ show ((==) adt exampleModel)

checkSolverExampleModel :: IO ()
checkSolverExampleModel = do
    putStrLn "Testing Solver with example model"
    putStrLn $ "> " ++ show (solveProblem exampleModel) ++ " <"
    putStrLn $ "> expected Just [(\"x\", 2)]"

mkFolderIfNotExists :: FilePath -> IO ()
mkFolderIfNotExists = createDirectoryIfMissing True

genOutputName :: FilePath -> String -> String -> FilePath
genOutputName outputFolder extension file = replaceExtension (replaceFolderAtIndex file 1 outputFolder) extension

replaceFolderAtIndex :: FilePath -> Int -> String -> FilePath
replaceFolderAtIndex filePath index newFolder =
    joinPath (replaceAtIndex index newFolder (splitDirectories dirPath))
        </> fileName
  where
    -- Split the file path into directory and file components
    dirPath = fst $ splitFileName filePath
    fileName = takeFileName filePath

    -- Replace the folder at the specified index using zipWith
    replaceAtIndex :: Int -> String -> [FilePath] -> [FilePath]
    replaceAtIndex idx newVal = zipWith (\i folder -> if i == idx then newVal else folder) [0 ..]

-- | The expected output file a given input is checked against.
expectedPath :: FilePath -> FilePath
expectedPath inputFile = "examples/expected_output/" ++ takeBaseName inputFile ++ ".out"

-- | Parse and solve one input file, writing the result to the output folder.
-- Returns 'Nothing' if the file could not be read or the model did not parse.
processFile :: FilePath -> FilePath -> IO (Maybe Solution)
processFile inputFile outputFile = do
    putStrLn $ "Starting " ++ inputFile
    result <- try (readFile inputFile) :: IO (Either IOException String)
    case result of
        Left e
            | isDoesNotExistError e -> do
                putStrLn $ "File not found: " ++ inputFile
                pure Nothing
            | otherwise -> do
                putStrLn $ "Error reading file " ++ inputFile ++ ": " ++ show e
                pure Nothing
        Right contents -> do
            case parse miniZincParser contents of
                Result _ adt -> do
                    let solution = solveProblem adt
                        solved = maybe "Nothing" show solution
                    writeFile outputFile (solved ++ "\n")
                    putStrLn $ "Processed: " ++ inputFile
                    pure (Just solution)
                _ -> do
                    putStrLn $ "Invalid ParseResult for file " ++ inputFile
                    pure Nothing

-- | Check one input's result against all of its accepted answers.
checkInput :: FilePath -> IO Bool
checkInput inputFile = do
    let actualOutputFile = genOutputName "output" "out" inputFile
    actual <- processFile inputFile actualOutputFile
    expectedLines <- lines <$> readFile (expectedPath inputFile)
    let ok = case actual of
            Nothing -> False
            Just solution -> isAccepted expectedLines solution
    putStrLn $
        "Checking "
            ++ inputFile
            ++ " against accepted solutions: "
            ++ (if ok then "PASS" else "FAIL")
    pure ok

main :: IO ()
main = do
    putStrLn "Testing "
    checkADTExampleModel
    checkSolverExampleModel

    putStrLn ""
    putStrLn "Testing example inputs:"

    inputFiles <- glob "examples/inputs/*.mzn"

    mkFolderIfNotExists "./examples/output"

    results <- forM inputFiles checkInput

    putStrLn ""
    putStrLn $
        "Matched an accepted solution for "
            ++ show (length (filter id results))
            ++ " of "
            ++ show (length results)
            ++ " example input(s)."

    putStrLn "\nAll processing complete."
