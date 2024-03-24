import Data.Char (toLower)
import Data.List.Split (wordsBy)
import qualified Data.Map as Map

-- | Process text and count occurrences of each word.
-- TODO bug: this function breaks when input contains punctuation marks; assigned OthelloEngineer
-- TODO feature: please have this accept nordic letters hashkell
processText :: String -> Map.Map String Int
processText input = Map.fromListWith (+) [(word, 1) | word <- wordsBy (not . isAlphaNum) $ map toLower input]
  where isAlphaNum c = c `elem` ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ "æøåÆØÅ"

main :: IO ()
main = do
  let text = "The quick brown fox jumps over the lazy dog"
  print $ processText text