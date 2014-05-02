import Data.List.Split

type Matrix = [[Integer]]

createMatrix :: Integer -> Matrix
createMatrix n = chunksOf (fromIntegral n) [1..n*n] 


right :: Matrix -> [Integer]
right = head

left :: Matrix -> [Integer]
left = reverse . init . last

up :: Matrix -> [Integer]
up =  reverse . tail . init . map head

down :: Matrix -> [Integer]
down = tail . map last

border :: Matrix -> [Integer]
border m = right m ++ down m ++ left m ++ up m

submatrix :: Matrix -> Matrix
submatrix m = map (tail . init) ((tail . init) m)

snail :: Matrix -> [Integer]
snail [] = []
snail [[a]] = [a]
snail m = border m ++ (snail $ submatrix m)
