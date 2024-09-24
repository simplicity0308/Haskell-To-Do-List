module Main where
import Text.CSV
import Data.List 
import Text.Read
import Data.Time

--define Task data type
data Task = Task {date :: String, task :: String} deriving (Eq)

--displayed menu variable
menu :: IO String
menu = putStrLn "*******************************" >>
          putStrLn "TO DO LIST" >>
            putStrLn "*******************************" >>
              putStrLn "Please enter a number: " >>
                putStrLn "" >>
                  putStrLn "1. Add Item To List" >>
                    putStrLn "2. Remove Item From List" >>
                      putStrLn "3. Display Current List" >>
                        putStrLn "4. Edit Item In Current List" >>
                          putStrLn "5. Quit" >>
                            putStrLn "" >>
                              putStrLn "" >>
                                getLine
                              
--handle user input in a loop
process :: String -> CSV-> IO()
process "1" csv = action1 filePath csv >> menu >>= process <*> return csv
process "2" csv = action2 filePath csv >> menu >>= process <*> return csv
process "3" csv = action3 filePath >> menu >>= process <*> return csv
process "4" csv = action4 filePath csv >> menu >>= process <*> return csv
process "5" csv = putStrLn "Goodbye!" 
process _  csv = putStrLn "" >> 
  putStrLn "Please Enter a valid number" >>
    putStrLn "" >>
      menu >>= process <*> return csv

--process csv file, define filepath as global variable
filePath = "Task.csv"
main = do
  values <- parseCSVFromFile "Task.csv"
  case values of
    Left err -> print err
    Right csv ->  do
      menu >>= process <*> return csv

--actions
--action1 : add task
action1 :: FilePath -> CSV -> IO()
action1 fp csv = addTask fp csv 

--action2 : remove task
action2 :: FilePath -> CSV -> IO()
action2 filePath csv = do
  putStrLn "== Removing Item =="
  removeTask filePath
  putStrLn ""

--action3 : display task
action3 :: FilePath -> IO()
action3 filePath = do
  putStrLn ""
  putStrLn "== Tasks =="
  values <- parseCSVFromFile filePath
  case values of
    Left err -> print err
    Right csv -> displayAll $ getTask csv
  putStrLn ""
  putStrLn ""

--action4 : edit task
action4 :: FilePath -> CSV -> IO ()
action4 filePath csv = editTask filePath csv

--function to parse and process CSV file 
getTask :: CSV -> [Task]
getTask csv = fmap (\rec -> Task {date = rec !! 0, task = rec !! 1}) (tail csv)

--instance of show to display Task
instance Show Task where
  show(Task {date = d, task = t}) = "Date: " ++ d ++ " ; Task: " ++ t

--function to display all tasks
displayAll :: [Task] -> IO()
displayAll tasks = mapM_ (\(num,task) -> putStrLn $ show num ++ " - " ++ show task) $ zip [1..] tasks
   
--function to add item to list
addTask :: FilePath -> CSV -> IO()
addTask filePath csvData = do
  putStrLn ""
  putStrLn ""
  putStrLn "Adding New Task"
  putStrLn "Enter date in DD/MM/YYYY format: "
  inputDate <- getLine
  if isValidDateFormat inputDate
    then do
      putStrLn ""
      putStrLn "Enter task: "
      inputTask <- getLine
      putStrLn ""
      putStrLn ""

      let newTask = [inputDate, inputTask]
      let updatedCSV = csvData ++ [newTask]
      let csvString = printCSV updatedCSV
      writeFile filePath csvString
      putStrLn ""
      putStrLn "== Task Successfully Added =="
      putStrLn ""
    else do
      putStrLn "== Invalid Date Syntax =="
      addTask filePath csvData

--function to check valid date format
isValidDateFormat :: String -> Bool
isValidDateFormat inputDate =
  case parseDate inputDate of
    Just _ -> True
    Nothing -> False

--parse date Str to day data type
parseDate :: String -> Maybe Day
parseDate = parseTimeM True defaultTimeLocale "%d/%m/%Y"

    
--function to remove item from csv
removeTask :: FilePath -> IO()
removeTask filePath = do
  values <- parseCSVFromFile filePath
  case values of
    Left err -> print err
    Right csv -> do
      let tasks = getTask csv
      displayAll $ tasks
      putStrLn ""
      putStrLn "Choose which task to remove: "
      lineToRemove <- getLine
      let selectedTask = readMaybe lineToRemove :: Maybe Int
      case selectedTask of
        Just taskIndex ->
          if taskIndex >= 1 && taskIndex <= length tasks
          then do
            let updatedCSV = removeLine (taskIndex + 1)csv
            let csvString = printCSV updatedCSV
            writeFile filePath csvString
            putStrLn ""
            putStrLn "== Task removal successs =="
            putStrLn ""
          else do 
            putStrLn "== Invalid Number. Please try again =="
            putStrLn ""
            removeTask filePath
        Nothing -> do
          putStrLn "== Input cannot be empty =="
          putStrLn ""
          removeTask filePath

--function to remove an entire line (including space)
removeLine :: Int -> CSV -> CSV
removeLine lineNo csv = delete (csv !! (lineNo - 1)) csv

--function to edit an existing record
editTask :: FilePath -> CSV -> IO ()
editTask filePath csv = do
  values <- parseCSVFromFile filePath
  case values of
    Left err -> print err
    Right csv -> do
      putStrLn "== Tasks =="
      displayAll (getTask csv)
      let loop = do
            putStrLn ""
            putStrLn "Choose which task to edit: "
            lineToEdit <- getLine
            let selectedTask = readMaybe lineToEdit :: Maybe Int

            case selectedTask of
              Just taskIndex ->
                if taskIndex >= 1 && taskIndex <= length csv
                  then do
                    let updatedCSV = removeLine (taskIndex + 1) csv
                    addTask filePath updatedCSV 
                    putStrLn "== Task edited successfully =="
                  else do
                    putStrLn "== Invalid Number. Please try again =="
                    loop 
              Nothing -> do
                putStrLn "== Input cannot be empty =="
                loop  
      loop


--add item to list
--remove item from list
--display current list
--edit item in list
