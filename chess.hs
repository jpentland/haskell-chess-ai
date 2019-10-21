import Data.List

-- Square specifies a square on the board
type Square = (Int, Int)

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

-- Get only pieces of a certain colour
getByColour :: Colour -> Board -> Board
getByColour c = filter $ (==c) . getColour

-- Get only pieces of a certain type
getByType :: PieceType -> Board -> Board
getByType t = filter $ (==t) . getType

-- Return opposite colour
flipColour c = if c == Black then White else Black

-- Piece describes a chess piece at a given location on the board
newtype Piece = Piece (PieceType, Colour, Square) deriving Eq
data PieceType = Rook | Knight | Bishop | Queen | King | Pawn deriving (Show, Eq)
data Colour = Black | White deriving (Show, Eq)

-- Pretty format for showing a piece
instance Show Piece where
        show (Piece (t, c, l)) = (show c) ++ " " ++ (show t) ++ " at " ++ (show l)

-- Get x coordinate of piece
getx :: Piece -> Int
getx (Piece (_, _, (x, _))) = x

-- Get y coordinate of piece
gety :: Piece -> Int
gety (Piece (_, _, (_, y))) = y

-- Get square piece is on
getsq :: Piece -> Square
getsq p = (getx p, gety p)

-- Get type of piece
getType :: Piece -> PieceType
getType (Piece (t, _, _)) = t

-- Get colour of piece
getColour :: Piece -> Colour
getColour (Piece (_, c, _)) = c

-- Apply a move to a piece
move :: Piece -> Move -> Piece
move (Piece (t, c, (x, y))) (Move (dx, dy)) = Piece (t, c, (x + dx, y + dy))

-- A board is just a list of Pieces
type Board = [Piece]

-- Create the same piece at multiple locations on the board
createPieces :: PieceType -> Colour -> [Square] -> Board
createPieces t c ss = map (\s -> (Piece (t, c, s))) ss

-- Get what piece is at a location
getPieceAt :: Board -> Square -> Maybe Piece
getPieceAt b s = find (isAt s) b
        where isAt s p = s == (getsq p)

-- Is the square empty
isEmpty :: Board -> Square -> Bool
isEmpty b s = maybeToBool $ getPieceAt b s
        where maybeToBool (Just _)  = False
              maybeToBool Nothing   = True

-- The way the board is set up at the start of the game
startingBoard :: Board
startingBoard = mirrorAll Black $
        (createPieces Rook   White [(0,0),(7,0)]) ++
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
getmoves Pawn   = []

-- Generate moves for a pawn
pawnMoves :: Colour -> Square -> [Move]
pawnMoves Black (_, 6) = [Move (0, -1), Move (0, -2)]
pawnMoves White (_, 1) = [Move (0, 1), Move (0, 2)]
pawnMoves Black _ = [Move (0, -1)]
pawnMoves White _ = [Move (0, 1)]

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
        | ((getType p) == Pawn) = stayOnBoard p $ pawnMoves (getColour p) (getsq p)
        | otherwise = stayOnBoard p $ getmoves $ getType p

-- canMove checks whether a raw move can work on a board with pieces
canMove :: Board -> Piece -> Move -> Bool
canMove b p m = canKillOrEmpty (getPieceAt b $ head path) && (all (isEmpty b) (tail path))
        where path = getPath p m
              canKillOrEmpty (Just p') = (getType p) /= Pawn && (getColour p) /= (getColour p')
              canKillOrEmpty Nothing = True

-- Return all squares that a piece needs to pass though to make a move, with the final square at the head of the list
getPath :: Piece -> Move -> [Square]
getPath (Piece (Knight, _, (x, y))) (Move (dx, dy)) = [(x+dx, y+dy)]
getPath p (Move (dx, dy)) = zip (extend n xs) (extend n ys)
        where dir v = if (v > 0) then (v - 1) else (v + 1)
              xs = [ (getx p) + x | x <- [dx,(dir dx)..0] ]
              ys = [ (gety p) + y | y <- [dy,(dir dy)..0] ]
              extend n xs = take n $ xs ++ (repeat (last xs))
              n = if (abs dx) > (abs dy) then (abs dx) else (abs dy)

-- Return all moves a piece could make on a board
getMoves :: Board -> Piece -> [Move]
getMoves b p = filter (canMove b p) $ (rawMoves p) ++ pawnAttack
    where
          pawnAttack = filter ((\m -> enemyAtSq m /= Nothing)) attackMove
          enemyAtSq m = do p' <- getPieceAt b $ getsq $ move p m
                           if (getColour p' /= getColour p) then return Nothing else return (Just p')
          attackMove
            | getType p == Pawn && getColour p == White = [(Move (1,1)), (Move (-1,1))]
            | getType p == Pawn && getColour p == Black = [(Move (1,-1)), (Move (-1,-1))]
            | otherwise = []

-- Check if p1 could take p2 in the next move
threat :: Board -> Piece -> Piece -> Bool
threat b p1 p2 = (getsq p2) `elem` ss
        where ss = map (getsq . (move p1)) $ getMoves b p1

-- Check if the king is under threat for a given colour
kingUnderThreat :: Colour -> Board -> Bool
kingUnderThreat c b = any (threat b king) (enemies)
        where king = ((getByColour c) . (getByType King)) b !! 0
              enemies = getByColour (flipColour c) b

-- Return all moves a player could make on a board
getPlayerMoves :: Colour -> Board -> [(Piece, [Move])]
getPlayerMoves c b = map (\p -> (p, getMoves b p)) usablePieces
    where
            playerPieces = getByColour c b
            usablePieces = if kingUnderThreat c b then (getByType King playerPieces) else playerPieces

-- Check for a checkmate
isCheckMate :: Colour -> Board -> Bool
isCheckMate c b = getPlayerMoves c b == []

-- Create a player score for a board
score :: Colour -> Board -> Int
score c b = foldr (+) 0 $ map (pieceScore c) b
    where
        pieceScore c p
          | (getColour p) == c  = pieceTypeScore $ getType p
          | otherwise           = negate $ pieceTypeScore $ getType p
        pieceTypeScore King     = 1000
        pieceTypeScore Queen    = 10
        pieceTypeScore Rook     = 6
        pieceTypeScore Bishop   = 5
        pieceTypeScore Knight   = 4
        pieceTypeScore Pawn     = 1

-- Apply a move to a board
applyMove :: Piece -> Move -> Board -> Board
applyMove p m b = (move p m) : (filter (\p' -> p /= p' && (not (killedByMove p m p'))) b)

-- True if piece is killed by another piece applying a move
-- TODO: En passant
killedByMove :: Piece -> Move -> Piece -> Bool
killedByMove p1 m p2 = (getsq $ move p1 m) == getsq p2

-- Show list of moves a player could make
showPlayerMoves :: Colour -> Board -> String
showPlayerMoves c b = concatMap (\(p, m) -> (show p) ++ ": " ++ (show m) ++ "\n") $ getPlayerMoves c b

board :: String
board = (take (8*6+2) $ cycle "_") ++ drawSquares ++ "\n" ++ (take (8*6+2) $ cycle "-")
        where drawSquares = concatMap drawLine $ take (8*3) $ cycle [Black, Black, Black, White, White, White]
              drawLine White = "\n|" ++ (concat $ take 8 $ cycle ["######","      "]) ++ "|"
              drawLine Black = "\n|" ++ (concat $ take 8 $ cycle ["      ","######"]) ++ "|"

-- Let players interact with game
-- TODO: Check for errors etc
boardLoop c b = do
        putStrLn $ "Score: " ++ (show (score c b))
        let moves = getPlayerMoves c b
        let ps = map (\(p, _) -> p) moves
        let renderLine i d = (show i) ++ ": " ++ (show d)
        putStrLn $ unlines $ zipWith renderLine [0..] moves
        num <- getLine
        let (p, ms) = moves !! (read num)
        putStrLn $ unlines $ zipWith renderLine [0..] ms
        num <- getLine
        let m = ms !! (read num)
        let nextColour = if c == White then Black else White
        boardLoop nextColour $ applyMove p m b

main = boardLoop White startingBoard
