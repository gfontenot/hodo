module Hodo where
import System.Environment (getArgs)
import Control.Applicative
import System.Directory (removeFile, renameFile)
import System.IO (hFlush, stdout, hClose, openTempFile, hPutStr)
import Control.Exception (bracketOnError)
import Data.List (delete)

data Operation = Operation Command Arguments Tasks
type Command = String
type Arguments = String
type Tasks = [String]

perform :: Operation -> Tasks
perform (Operation "add" newTask tasks) = tasks ++ [newTask]
perform (Operation "complete" number tasks) = tasks `remove` (read number)
perform (Operation _ _ tasks) = tasks

remove :: Eq a => [a] -> Int -> [a]
remove xs i = delete (xs !! i) xs

listItems :: Tasks -> IO Tasks
listItems tasks = do
    mapM_ putStrLn $ numbered tasks
    return tasks

numbered :: [String] -> [String]
numbered = zipWith (\n line -> show n ++ " - " ++ line) [0..]

getPrompt :: IO String
getPrompt = do
    putStr "> "
    hFlush stdout
    getLine

main :: IO ()
main = do
    filename <- unwords <$> getArgs
    tasks <- lines <$> readFile filename
    (command:args) <- words <$> getPrompt
    newTasks <- listItems . perform $ Operation command (unwords args) tasks
    writeFile filename $ unlines newTasks
    putStrLn ""
    main
