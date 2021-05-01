module Ex3Main where

import Data.List (genericLength, intersperse)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (filterM)
import Result
import ParserCombinators
import Ex3
import GHC.IO.IOMode
import System.IO (hGetChar, hIsEOF, hClose, openFile)
import Data.Typeable
import Control.Exception

{- 3.3.0 -}

{-
I COMPLETED THIS PART IN A WAY THAT USER IS ASKED INPUTS AFTER RUNNING THE MAIN METHOD
It can filter inputs, display only specific fields, perform aggregations (+, *, average) and output filtered result as an html table

EXAMPLE

> main
"Enter formula"

(< (intOfString height) 1250)

"Enter CSV file"

mountainsFixed.csv

"Would you like to display all fileds? enter 'yes' if you want to see a whole filtered file or 'no' to choose specific fields"

no

"Please enter fields you want to display (ex. height,name)"

name

name
Cairn Gorm
Aonach Beag
Aonach Mòr
Càrn Mòr Dearg
Ben Lawers
Beinn a' Bhùird
Beinn Mheadhoin
"Do you want to perform aggregation on this data? enter 'no' or 'yes'"

yes

"Enter what aggregation you want to perform (eg. +) (options: +, *, average)"

*

"Enter initial value to start aggregation from"

1

"Enter field to perform aggregation on (only numeric)"

height

3930988327541264808000
"Do you want to display this data as an HTML table? enter 'no' or 'yes'"

yes

<HTML>
<BODY>
<table>
   <tr>
     <th>name</th>
   </tr>
   <tr>
     <th>Cairn Gorm</th>
   </tr>
   <tr>
     <th>Aonach Beag</th>
   </tr>
   <tr>
     <th>Aonach Mòr</th>
   </tr>
   <tr>
     <th>Càrn Mòr Dearg</th>
   </tr>
   <tr>
     <th>Ben Lawers</th>
   </tr>
   <tr>
     <th>Beinn a' Bhùird</th>
   </tr>
   <tr>
     <th>Beinn Mheadhoin</th>
   </tr>
</table>
</BODY>
</HTML>

-}

main :: IO ()
main = 
  do print "Enter formula"
     formula      <- getLine
     print "Enter CSV file"
     csv          <- getLine
     file         <- readFile csv
     print "Would you like to display all fileds? enter 'yes' if you want to see a whole filtered file or 'no' to choose specific fields"
     filterChoice <- getLine
     case filterChoice of
      "no" -> do print "Please enter fields you want to display (ex. height,name)"
                 desiredFields <- getLine
                 putStr (stringOfCSVFile (filterByField (fromResToValArray (runParser parseArray desiredFields)) (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file)))))
                 print "Do you want to perform aggregation on this data? enter 'no' or 'yes'"
                 aggreagationChoice <- getLine
                 case aggreagationChoice of
                    "yes" -> do print "Enter what aggregation you want to perform (eg. +) (options: +, *, average)"
                                operation <- getLine
                                print "Enter initial value to start aggregation from"
                                initValue <- getLine
                                print "Enter field to perform aggregation on (only numeric)"
                                field     <- getLine
                                case operation of
                                  "+"       -> print (foldl (+) (toInteger (stringToInt initValue)) (fromRecordsToArray (filterByFieldsRecords [field] (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file))))))
                                  "*"       -> print (foldl (*) (toInteger (stringToInt initValue)) (fromRecordsToArray (filterByFieldsRecords [field] (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file))))))
                                  "average" -> print (realToFrac (sum (fromRecordsToArray (filterByFieldsRecords [field] (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file))))))  / genericLength  (fromRecordsToArray (filterByFieldsRecords [field] (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file))))) )
                    _  -> print "Thanks for using this program!"
                 print "Do you want to display this data as an HTML table? enter 'no' or 'yes'"
                 hTMLchoice <- getLine
                 case hTMLchoice of
                    "yes" -> putStr (csvFileIntoHTML (filterByField (fromResToValArray (runParser parseArray desiredFields)) (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file)))))
                    _     -> print "Thanks for using this program!"
      _     -> do putStr (stringOfCSVFile (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file))))
                  print "Do you want to perform aggregation on this data? enter 'no' or 'yes'"
                  aggreagationChoice <- getLine
                  case aggreagationChoice of
                      "yes" -> do print "Enter what aggregation you want to perform (eg. +) (options: +, *, average)"
                                  operation <- getLine
                                  print "Enter initial value to start aggregation from"
                                  initValue <- getLine
                                  print "Enter field to perform aggregation on (only numeric)"
                                  field <- getLine
                                  case operation of
                                    "+"       -> print (foldl (+) (toInteger (stringToInt initValue)) (fromRecordsToArray (filterByFieldsRecords [field] (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file))))))
                                    "*"       -> print (foldl (*) (toInteger (stringToInt initValue)) (fromRecordsToArray (filterByFieldsRecords [field] (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file))))))
                                    "average" -> print (realToFrac (sum (fromRecordsToArray (filterByFieldsRecords [field] (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file))))))  / genericLength  (fromRecordsToArray (filterByFieldsRecords [field] (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file))))))
                      _  -> print "Thanks for using this program!"
                  print "Do you want to display this data as an HTML table? enter 'no' or 'yes'"
                  hTMLchoice <- getLine
                  case hTMLchoice of
                    "yes" -> putStr (csvFileIntoHTML (filterCSV (fromResToValExpr (runParser parseExpr formula)) (fromResToValCSV (runParser parseCSVFile file))))
                    _     -> print "Ok!"


parseArray :: Parser FieldNames
parseArray = sepBy comma1 parseCSVField
 
comma1 :: Parser()
comma1 = do spaces
            isChar ','
            spaces

fromRecordsToArrayOne :: CSVRecord -> [Integer]
fromRecordsToArrayOne = map (toInteger . stringToInt)

fromRecordsToArray :: [CSVRecord] -> [Integer]
fromRecordsToArray = concatMap fromRecordsToArrayOne

-- (< (intOfString height) 1300)
-- "name,height\nBen Nevis,1345\nBen Macdui,1309\nBraeriach,1296\nCairn Toul,1291\n"
--(== (intOfString height) 1345) 

-- filterCSV (fromResToValExpr (runParser parseExpr "(< (intOfString height) 1300)")) (fromResToValCSV (runParser parseCSVFile "name,height\nBen Nevis,1345\nBen Macdui,1309\nBraeriach,1296\nCairn Toul,1291\n"))

-- WORKING TESTS
-- filterCSV (Opr LessThan [Opr IntOfString [Var "height"],Value (IntValue 1300)]) (["name", "height"], [["Ben Nevis", "1345"], ["Ben Macdui", "1309"], ["Braeriach", "1296"], ["Cairn Toul", "1291"]])
-- filterCSV (IfThenElse (Opr LessThan [Opr IntOfString [Var "height"],Value (IntValue 1300)]) (Value (BoolValue False)) (Value (BoolValue True))) (["name", "height"], [["Ben Nevis", "1345"], ["Ben Macdui", "1309"], ["Braeriach", "1296"], ["Cairn Toul", "1291"]])
--here doesn't matter what expression is, it is dealt with in filterCSVRecord
filterCSV :: Expr -> CSVFile -> CSVFile
filterCSV exp (fieldNames,records) = (fieldNames,filterCSVRecords exp fieldNames records)

--formula that gives something back can only be operation or if statement
filterCSVRecords :: Expr -> FieldNames -> [CSVRecord] -> [CSVRecord]
filterCSVRecords (Opr op exps) fn (r:records)                      = if evalOperation op (intoValues exps fn r) == Ok(BoolValue True) then r : filterCSVRecords (Opr op exps) fn records else filterCSVRecords (Opr op exps) fn records
--if first true, call this function on first exp, on second otherwise
filterCSVRecords (IfThenElse (Opr op exps) ex2 ex3) fn (r:records) = if evalOperation op (intoValues exps fn r) == Ok(BoolValue True) then filterCSVRecords ex2 fn [r] ++ filterCSVRecords (IfThenElse (Opr op exps) ex2 ex3) fn records else filterCSVRecords ex3 fn [r] ++ filterCSVRecords (IfThenElse (Opr op exps) ex2 ex3) fn records
filterCSVRecords (Value (BoolValue True)) fn (r:records)           = r : filterCSVRecords (Value (BoolValue True)) fn records
filterCSVRecords _ _ _                                             = []

--returns arguments with substituted values from record
intoValues :: [Expr] -> FieldNames -> CSVRecord -> [Value]
intoValues [] fn record                  = []
intoValues (Value a:exp) fn record       = a : intoValues exp fn record
intoValues ((Var a):exp) fn record       = getValueInCSV a fn record : intoValues exp fn record
intoValues ((Opr op exps):exp) fn record =  workUntilValue (Opr op exps) fn record : intoValues exp fn record

--finds needed value in the record
getValueInCSV :: String -> FieldNames -> CSVRecord -> Value
getValueInCSV field (x:xs) (y:ys) = if(field == x) then StringValue y else getValueInCSV field xs ys
getValueInCSV field _ _           = StringValue "Error"

--evaluate operation on function until it's value
workUntilValue:: Expr -> FieldNames -> CSVRecord -> Value
workUntilValue (Value a) _ _          = a
workUntilValue (Opr op exp) fn record = fromResToVal (evalOperation op (intoValues exp fn record))

fromResToVal :: Result Value -> Value
fromResToVal (Ok a) = a
fromResToVal _      = BoolValue False

fromResToValExpr :: Result (Expr, String) -> Expr
fromResToValExpr (Ok (a, s)) = a 

fromResToValCSV :: Result (CSVFile, String) -> CSVFile
fromResToValCSV (Ok (a, s)) = a 

fromResToValArray :: Result ([String], String) -> [String]
fromResToValArray (Ok (a, "")) = a
fromResToValArray _            = [] 

--this function takes desired fields and a csv file and outputs csv file only with the fields that user specified
--filterByField ["name"] (["name", "height"], [["Ben Nevis", "1345"], ["Ben Macdui", "1309"], ["Braeriach", "1296"], ["Cairn Toul", "1291"]]) == (["name"],[["Ben Nevis"],["Ben Macdui"],["Braeriach"],["Cairn Toul"]])
filterByField :: FieldNames -> CSVFile -> CSVFile
filterByField wantedFn (fn, records) = (wantedFn, filterByFieldsRecords wantedFn (fn, records))

filterByFieldsRecords :: FieldNames -> CSVFile -> [CSVRecord]
filterByFieldsRecords wantedFn (fn, r:records) = filterByFieldsOneRecord wantedFn fn r : filterByFieldsRecords wantedFn (fn, records)
filterByFieldsRecords _ _                      = []

filterByFieldsOneRecord :: FieldNames -> FieldNames-> CSVRecord -> CSVRecord
filterByFieldsOneRecord wantedfieldNames (fn:fieldNames) (r:records) = if (isWantedField wantedfieldNames fn) then r : filterByFieldsOneRecord wantedfieldNames fieldNames records else filterByFieldsOneRecord wantedfieldNames fieldNames records
filterByFieldsOneRecord _ _ _                                        = []

isWantedField :: FieldNames -> String -> Bool
isWantedField (fn:fieldNames) s = if fn == s then True else isWantedField fieldNames s
isWantedField [] s              = False

csvFileIntoHTML :: CSVFile -> String
csvFileIntoHTML ([],[])         = "</table>" ++ "\n" ++ "</BODY>"  ++ "\n" ++ "</HTML>"++ "\n"
csvFileIntoHTML ([], r:records) = "   " ++ "<tr>" ++ "\n" ++ recordIntoHTML r ++ "   " ++ "</tr>" ++ "\n" ++ csvFileIntoHTML ([], records)
csvFileIntoHTML (fn, records)   = "<HTML>" ++ "\n" ++ "<BODY>" ++ "\n" ++ "<table>" ++ "\n" ++ "   " ++ "<tr>" ++ "\n" ++ fieldsIntoHTML fn ++"   " ++ "</tr>" ++ "\n" ++ csvFileIntoHTML ([], records)

fieldsIntoHTML :: FieldNames -> String
fieldsIntoHTML (f:fn) = "     " ++"<th>" ++ f ++ "</th>" ++ "\n" ++ fieldsIntoHTML fn
fieldsIntoHTML _      = ""

recordIntoHTML :: CSVRecord -> String
recordIntoHTML (f:fn) = "     " ++ "<td>" ++ f ++ "</td>" ++ "\n" ++ recordIntoHTML fn
recordIntoHTML _      = ""