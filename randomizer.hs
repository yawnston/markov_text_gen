import Data.Map (Map)
import Data.Char
import qualified Data.Map as Map
import System.Random
import Debug.Trace
import System.Environment


type FrequencyMap = Map String [(String, Int)]

-- make a list of pairs from a list, e.g. [1..4] -> [(1,2), (2,3), (3,4)]
makePairs :: [a] -> [(a, a)]
makePairs l = zip l (tail l)

-- predicate for checking whether a given word is already present as a next word in another word's record
checkRecordPresence :: String -> Bool -> (String, Int) -> Bool
checkRecordPresence _ True _ = True
checkRecordPresence w False (word, _) = word == w

-- list is guaranteed not to be empty by caller function
incrementWordInRecord :: [(String, Int)] -> String -> [(String, Int)]
incrementWordInRecord record w
    | not isInRecord = ((w, 1) : record)
    | otherwise = map (\(word, count) -> if word == w then (word, count + 1) else (word, count)) record
    where isInRecord = foldl (checkRecordPresence w) False record

putWord :: FrequencyMap -> (String, String) -> FrequencyMap
putWord freqMap (w, next)
    -- we haven't seen this word yet -> add an entry to the map
    | record == Nothing = Map.insert w [(next, 1)] freqMap
    -- we've seen this word -> update its record
    | otherwise = Map.insert w (incrementWordInRecord recordVal next) freqMap
    where record = Map.lookup w freqMap
          (Just recordVal) = record

-- create a FrequencyMap from a list of strings
getFrequencies :: [String] -> FrequencyMap
getFrequencies words = foldl (putWord) (Map.fromList []) pairs
    where pairs = [(x, y)| (x, y) <- makePairs words]

-- picks a random element from a list (unweighted)
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

-- picks a random string from a list of weighted strings (taking the weights into account)
pickWeighted :: [(String,Int)] -> IO String
pickWeighted xs = pickWeightedImpl xs 0 ""

-- single iteration weighted pick algorithm (very handy when our FrequencyMap can contain hundreds/thousands of records)
pickWeightedImpl :: [(String,Int)] -> Int -> String -> IO String
pickWeightedImpl [] _ selected = do return selected
pickWeightedImpl ((item, weight):xs) totalWeight selected = do
    rand <- randomRIO (0, totalWeight + weight)
    if rand >= totalWeight then pickWeightedImpl xs (totalWeight + weight) item
    else pickWeightedImpl xs (totalWeight + weight) selected

-- gets a random word that starts with an uppercase letter (unweighted)
getFirstWord :: FrequencyMap -> IO String
getFirstWord freqData = do
    let candidates = Map.toList $ Map.filterWithKey (\k a -> isUpper (head k)) freqData
    (w, record) <- pick candidates
    return w

-- predicate defining characters that are considered to end a sentence
isSentenceEnder :: Char -> Bool
isSentenceEnder c
    | c == '.' || c == '!' || c == '?' = True
    | otherwise = False

-- given a word, chooses a random one that can follow it
chooseNextWord :: FrequencyMap -> String -> IO String
chooseNextWord freqData prevWord = do
    let candidates = freqData Map.! prevWord

    {-
    -- this choice is not weighted, a weighted choice algorithm is used instead
    (w, count) <- pick candidates
    return w
    -}

    w <- pickWeighted candidates
    return w

makeSentenceBody :: FrequencyMap -> String -> IO String
makeSentenceBody freqData prevWord = do
    if isSentenceEnder (last prevWord) then return "" -- end of sentence -> don't add anything else
    else do
        nextWord <- chooseNextWord freqData prevWord
        body <- makeSentenceBody freqData nextWord
        return (nextWord ++ " " ++ body)

-- generate a full sentence, from start to finish
makeSentence :: FrequencyMap -> IO String
makeSentence freqData = do
    firstWord <- getFirstWord freqData
    body <- makeSentenceBody freqData firstWord
    return $ firstWord ++ " " ++ body

-- simulates a for loop, calling itself recursively to generate a new sentence (I had issues with using iterate inside a do block)
makeText :: FrequencyMap -> Int -> String -> IO String
makeText freqData count text = do
    if count <= 0 then return text
    else do
        sentence <- makeSentence freqData
        rest <- makeText freqData (count - 1) (text ++ " " ++ sentence)
        return rest

-- calling syntax: <programName> <fileName> <sentenceCount>
main = do
    -- outLength is the number of sentences to be printed
    (fileName : outLength : restArgs) <- getArgs
    fileContents <- readFile fileName
    let freqData = getFrequencies $ words [x | x <- fileContents]
    -- if we don't have any data to generate from, do nothing
    if freqData == (Map.fromList []) then putStr ""
    else do
        text <- makeText freqData (read outLength :: Int) ""
        putStr text