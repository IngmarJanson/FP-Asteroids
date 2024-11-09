module Score where

import Model

--Gets the highscores from the high scores file
getHighScores :: IO ScoreList
getHighScores =  do
                contents <- readFile "HighScores.txt"
                return $ map words $ lines contents

-- Checks if the score exceeds one of the top five high scores and adds it to the list deleting the last one
updateTopFiveHighScores :: ScoreList -> (String, Int) -> ScoreList
updateTopFiveHighScores scores@(x:xs) (newName, newScore) = removeScore (replaceScore scores (newName, newScore))

updateHighScores :: ScoreList -> PlayerInfo -> String
updateHighScores scores (PlayerInfo _ _ _ _ _ score name) = scoresToString $ updateTopFiveHighScores scores (name, score)

removeScore :: ScoreList -> ScoreList
removeScore scores = if length scores > 5 then init scores else scores

replaceScore :: ScoreList -> (String, Int) -> ScoreList
replaceScore [] _ = []
replaceScore (x@[name, score]:xs) (newName, newScore)
            | newScore > read score = [newName ++ " " ++ show newScore] : x : xs
            | otherwise = x : replaceScore xs (newName, newScore)

scoresToString :: ScoreList -> String
scoresToString scores = unlines $ map unwords scores