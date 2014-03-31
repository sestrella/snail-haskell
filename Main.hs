type Matrix = [[Integer]]

createMatrix :: Integer -> Matrix
createMatrix n = [ createRow (x * n) n | x <-[0..(n - 1)] ]

createRow :: Integer -> Integer -> [Integer]
createRow i l = [ x | x <-[(i + 1)..(i + l)] ]

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
