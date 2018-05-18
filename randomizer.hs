import Data.Map (Map)
import Data.Char
import qualified Data.Map as Map
import System.Random
import Debug.Trace
import System.Environment


type FrequencyMap = Map String [(String, Int)]

makePairs :: [a] -> [(a, a)]
makePairs l = zip l (tail l)

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

getFrequencies :: [String] -> FrequencyMap
getFrequencies words = foldl (putWord) (Map.fromList []) pairs
    where pairs = [(x, y)| (x, y) <- makePairs words]

-- picks a random element from a list (unweighted)
pick :: [a] -> IO a
pick xs = fmap (xs !!) $ randomRIO (0, length xs - 1)

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

chooseNextWord :: FrequencyMap -> String -> IO String
chooseNextWord freqData prevWord = do
    let candidates = freqData Map.! prevWord
    -- HACK: does not choose weighted (chooses unweighted)
    (w, count) <- pick candidates
    return w

makeSentenceBody :: FrequencyMap -> String -> IO String
makeSentenceBody freqData prevWord = do
    if isSentenceEnder (last prevWord) then return "" -- end of sentence -> don't add anything else
    else do
        nextWord <- chooseNextWord freqData prevWord
        body <- makeSentenceBody freqData nextWord
        return (nextWord ++ " " ++ body)

makeSentence :: FrequencyMap -> IO String -- FIXME: empty input?
makeSentence freqData = do
    firstWord <- getFirstWord freqData
    body <- makeSentenceBody freqData firstWord
    return $ firstWord ++ " " ++ body

makeText :: FrequencyMap -> Int -> String -> IO String
makeText freqData count text = do
    if count <= 0 then return text
    else do
        sentence <- makeSentence freqData
        rest <- makeText freqData (count - 1) (text ++ " " ++ sentence)
        return rest

main = do
    -- outLength is the number of sentences to be printed
    (fileName : outLength : restArgs) <- getArgs
    fileContents <- readFile fileName
    let freqData = getFrequencies $ words [x | x <- fileContents]
    text <- makeText freqData (read outLength :: Int) ""
    putStr text