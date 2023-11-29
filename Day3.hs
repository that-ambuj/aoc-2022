import Data.Char
import Data.List

main :: IO ()
main = interact $ show . solve2 . lines

solve1 :: [String] -> Int
solve1 = sum . map solveLine

solve2 :: [String] -> Int
solve2 = sum . map solveLine2 . split3

solveLine2 :: [String] -> Int
solveLine2 [x, y, z] = priority . head $ intersect3 x y z
solveLine2 _ = undefined

solveLine :: String -> Int
solveLine xs = priority . head . intersect' $ splitAt (length xs `div` 2) xs

intersect' :: (Eq a) => ([a], [a]) -> [a]
intersect' (xs, ys) = xs `intersect` ys

priority :: Char -> Int
priority c
  | isAsciiUpper c = ord c - 38
  | otherwise = ord c - 96

split3 [] = []
split3 xs = take 3 xs : split3 (drop 3 xs)

intersect3 xs ys zs = intersect zs $ intersect xs ys
