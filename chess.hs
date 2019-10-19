import Data.List

-- Move spcifies a Move a piece could make
newtype Move = Move (Int, Int) deriving (Show, Eq)

-- apply allows combinations of rotateMove flipxMove, flipyMove and extendMove
apply :: (a -> [a]) -> [a] -> [a]
apply f xs = foldr ((++) . f) [] xs

-- rotateMove returns all posible rotations of a move
rotateMove :: Move -> [Move]
rotateMove (Move (x, y)) = [Move (x, y), Move (y, negate x), Move (negate x, negate y), Move (negate y, x) ]

-- flipxMove flips a move on the x axis
flipxMove :: Move -> [Move]
flipxMove (Move (x, y)) = [Move (x, y), Move (x, negate y)]

-- flipyMove flips a move on the y axis
flipyMove :: Move -> [Move]
flipyMove (Move (x, y)) = [Move (x, y), Move (negate x, y)]

-- extendMove returns all possible moves a piece could make by extending a base move up to n squares
extendMove :: Int -> Move -> [Move]
extendMove n (Move (x, y)) = take n [ (Move (x*n, y*n)) | n <- [1..] ]

-- Piece describes a chess piece at a given location on the board
newtype Piece = Piece (PieceType, Colour, (Int, Int)) deriving (Show)
data PieceType = Rook | Knight | Bishop | Queen | King | Pawn deriving (Show, Eq)
data Colour = Black | White deriving (Show)

-- Get x coordinate of piece
getx :: Piece -> Int
getx (Piece (_, _, (x, _))) = x

-- Get y coordinate of piece
gety :: Piece -> Int
gety (Piece (_, _, (_, y))) = y

-- Get type of piece
gettype :: Piece -> PieceType
gettype (Piece (t, _, _)) = t

-- Get colour of piece
getcolour :: Piece -> Colour
getcolour (Piece (_, c, _)) = c

-- Apply a move to a piece
move :: Piece -> Move -> Piece
move (Piece (t, c, (x, y))) (Move (dx, dy)) = Piece (t, c, (x + dx, y + dy))

-- A board is just a list of Pieces
type Board = [Piece]

-- Create the same piece at multiple locations on the board
createPieces :: PieceType -> Colour -> [(Int, Int)] -> Board
createPieces t c xs = map (\l -> (Piece (t, c, l))) xs

startingBoard = mirrorAll Black $
        (createPieces Rook   White [(0,0),(0,7)]) ++
        (createPieces Knight White [(1,0),(6,0)]) ++
        (createPieces Bishop White [(2,0),(5,0)]) ++
        (createPieces Queen  White [(3,0)]) ++
        (createPieces King   White [(4,0)]) ++
        (createPieces Pawn   White [(x, 1) | x <- [0..7]])
        where
                mirrorAll c b = b ++ map (mirror1 c) b
                mirror1 c (Piece (t, _, (x, y))) = Piece (t, c, (7-x, 7-y))

-- Generate moves pieces can make
getmoves :: PieceType -> [Move]
getmoves Rook   = (extendMove 8) `apply` rotateMove (Move (0, 1))
getmoves Knight = flipxMove `apply` rotateMove (Move (1, 2))
getmoves Bishop = (extendMove 8) `apply` rotateMove (Move (1, 1))
getmoves Queen  = (getmoves Bishop) ++ (getmoves Rook)
getmoves King   = rotateMove (Move (0, 1)) ++ rotateMove (Move (1,1))
getmoves Pawn   = [Move (1, 0), Move (2, 0)]

-- onBoard checks if a piece would still be on the board after moving it
onBoard :: Piece -> Move -> Bool
onBoard p m = (getx p') `elem` [0..7] && (gety p') `elem` [0..7]
            where p' = move p m

-- stayOnBoard filters a list of moves to make sure they would still be on the board after moving
stayOnBoard :: Piece -> [Move] -> [Move]
stayOnBoard p xs = filter (onBoard p) xs

-- rawMoves gets the moves a piece could make if there were no other pieces on the board
rawMoves :: Piece -> [Move]
rawMoves p
        | ((gettype p) == Pawn) = stayOnBoard p (if (gety p == 1) then (getmoves Pawn) else [Move (1, 0)])
        | otherwise = stayOnBoard p $ getmoves $ gettype p

