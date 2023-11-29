{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import Data.Text as T (lines, pack, splitOn, unpack, words)

main :: IO ()
main = interact $ show . solve

solve :: String -> Integer
solve =
  sum . take 3 . reverse . sort
    . map (sum . map (read . T.unpack) . T.lines)
    . splitOn "\n\n"
    . pack
