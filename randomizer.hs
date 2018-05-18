import Data.Map (Map)
import qualified Data.Map as Map
import System.Random


checkRecordPresence :: String -> Bool -> (String, Integer) -> Bool
checkRecordPresence _ True _ = True
checkRecordPresence w False (word, _) = word == w

-- list is guaranteed not to be empty by caller function
incrementWordInRecord :: [(String, Integer)] -> String -> [(String, Integer)]
incrementWordInRecord record w = 
    let isInRecord = foldl (checkRecordPresence w) False record
    if isInRecord then ((w, 1) : record)
    else map (\(word, count) -> if word == w then (word, count + 1) else (word, count)) record

putWord :: Map String [(String, Integer)] -> String -> Map String [(String, Integer)]
putWord freqMap w
    -- we haven't seen this word yet -> add an entry to the map
    | record = Nothing = Map.insert w [(w, 1)] freqMap
    | otherwise = Map.insert w (incrementWordInRecord record w) freqMap
    where record = Map.lookup w freqMap

getFrequencies :: [String] -> Map String [(String, Integer)]
getFrequencies words = foldl (putWord) (Map.fromList []) words