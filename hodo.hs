module Hodo where
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
perform (Operation "complete" number tasks) = delete (tasks !! (read number)) tasks
perform (Operation _ _ tasks) = tasks

write :: Tasks -> IO ()
write tasks = do
    bracketOnError (openTempFile "." "temp")
        (\(tempName, tempHandle) -> do
            hClose tempHandle
            removeFile tempName)
        (\(tempName, tempHandle) -> do
            hPutStr tempHandle $ unlines tasks
            hClose tempHandle
            removeFile "todo.txt"
            renameFile tempName "todo.txt")

listItems :: Tasks -> IO Tasks
listItems tasks = do
    let numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] tasks
    mapM_ putStrLn numberedTasks
    return tasks

getPrompt :: IO String
getPrompt = do
    putStr "> "
    hFlush stdout
    getLine

main :: IO ()
main = do
    tasks <- lines <$> readFile "todo.txt"
    (command:args) <- words <$> getPrompt
    newTasks <- listItems . perform $ Operation command (unwords args) tasks
    write newTasks
    putStrLn ""
    main
