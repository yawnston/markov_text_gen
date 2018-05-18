import Data.Map (Map)
import qualified Data.Map as Map
import System.Random
import Debug.Trace
import System.Environment


makePairs :: [a] -> [(a, a)]
makePairs l = zip l (tail l)

checkRecordPresence :: String -> Bool -> (String, Integer) -> Bool
checkRecordPresence _ True _ = True
checkRecordPresence w False (word, _) = word == w

-- list is guaranteed not to be empty by caller function
incrementWordInRecord :: [(String, Integer)] -> String -> [(String, Integer)]
incrementWordInRecord record w
    | not isInRecord = ((w, 1) : record)
    | otherwise = map (\(word, count) -> if word == w then (word, count + 1) else (word, count)) record
    where isInRecord = foldl (checkRecordPresence w) False record

putWord :: Map String [(String, Integer)] -> (String, String) -> Map String [(String, Integer)]
putWord freqMap (w, next)
    -- we haven't seen this word yet -> add an entry to the map
    | record == Nothing = Map.insert w [(next, 1)] freqMap
    -- we've seen this word -> update its record
    | otherwise = Map.insert w (incrementWordInRecord recordVal next) freqMap
    where record = Map.lookup w freqMap
          (Just recordVal) = record

getFrequencies :: [String] -> Map String [(String, Integer)]
getFrequencies words = foldl (putWord) (Map.fromList []) pairs
    where pairs = [(x, y)| (x, y) <- makePairs words]

main = do
    (fileName : outLength : restArgs) <- getArgs
    fileContents <- readFile fileName
    let freqData = getFrequencies $ words [x | x <- fileContents]
    putStr $ show freqData