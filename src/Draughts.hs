{-# LANGUAGE MultiParamTypeClasses #-}
-- | Logic of Draughts board game
module Draughts where

import Game.Board.BasicTurnGame

-- | Piece to draughts - Man or King
data Piece    = Man | King deriving(Eq)
-- | Tile of the Board - white or black
data Tile     = TileBlack | TileWhite deriving(Eq)
-- | Player of game - Black or White
data Player   = Black | White deriving (Eq)
-- | LastMove in this turn - Captured piece or None
data LastMove = Captured (Int,Int) | None deriving(Eq)

-- | Data of a draughts game
data DraughtsGame = DraughtsGame LastMove (GameState Int Tile Player Piece)

-- | DraughtsGame default initialization
defaultDraughtsGame :: DraughtsGame
defaultDraughtsGame = DraughtsGame None $ GameState
 { curPlayer'      = White
 , boardPos        = allTiles
 , boardPieces'    = pieces
 }
 where allTiles = [(x,y,TileWhite) | x <- [1..8], y <- [1..8], (x `mod` 2 == y `mod` 2)] ++
         [(x,y,TileBlack) | x <- [1..8], y <- [1..8], (x `mod` 2 /= y `mod` 2)]
       pieces   = [(x,y,Black,Man) | x <- [1..8], y<-[6..8], (x `mod` 2 == 0 && y==7) || (x `mod` 2 == 1 && y/=7) ] ++
         [(x,y,White,Man) | x <- [1..8], y<-[1..3], (x `mod` 2 == 1 && y==2) || (x `mod` 2 == 0 && y/=2) ]

-- | TooglePlayer, reset LastMove
togglePlayer (DraughtsGame _ (GameState Black boardPos boardPieces')) = (DraughtsGame None (GameState White boardPos boardPieces'))
togglePlayer (DraughtsGame _ (GameState White boardPos boardPieces')) = (DraughtsGame None (GameState Black boardPos boardPieces'))

-- | If any piece fo current player can capture anything
canAnyCapture (DraughtsGame _ game) = or (map (\(x,y,player,piece) ->
    if(player == curPlayer' game) then canCapture (x,y,player,piece) game
    else False) (boardPieces' game))

-- | If this piece can capture anytning
canCapture piece game =
    or (map (\p -> canCaptureThis piece p game) (boardPieces' game))

-- | If this piece can capture that piece
canCaptureThis (x,y,player,Man) (x2,y2,player2,piece2) game =
    if(player==player2 || x2==1 || x2==8 || y2==1 || y2==8) then False
    else abs (x -x2) == 1 && abs(y - y2) == 1 &&  not(hasPiece game (2*x2-x, 2*y2-y))
canCaptureThis (x,y,player,King) (x2,y2,player2,piece2) game =
    if(player==player2 || x2==1 || x2==8 || y2==1 || y2==8 || abs (x -x2) /= abs(y - y2)) then False
    else
      if(x<x2 && y<y2)      then not ( or (map (\(x,y) -> hasPiece game (x,y)) (zip [x+1..x2-1] [y+1..y2-1]))) && not (hasPiece game (x2+1,y2+1))
      else if(x>x2 && y>y2) then not ( or (map (\(x,y) -> hasPiece game (x,y)) (zip [x2+1..x-1] [y2+1..y-1]))) && not (hasPiece game (x2-1,y2-1))
      else if(x>x2 && y<y2) then not ( or (map (\(x,y) -> hasPiece game (x,y)) (zip [x2+1..x-1] [y+1..y2-1]))) && not (hasPiece game (x2-1,y2+1))
      else                       not ( or (map (\(x,y) -> hasPiece game (x,y)) (zip [x+1..x2-1] [y2+1..y-1]))) && not (hasPiece game (x2+1,y2-1))



-- | Checking move of King
kingsMove (DraughtsGame last game) _player posO posA posD
    | posD == posN =
        if(not (hasPiece game posD) && not (canAnyCapture (DraughtsGame last game)))
           then [MovePiece posO posD]
           else []
    | Just (player, _) <- getPieceAt game posN =
        if (player /= _player)
         then kingsCapture game _player posO posN posD posN
         else []
    | otherwise = kingsMove (DraughtsGame last game) _player posO posN posD
  where posN = ((fst posA + signum (fst posD - fst posO)), snd posA + signum (snd posD - snd posO))

-- | Checking move of King which captured piece already
kingsCapture game _player posO posA posD posE
    | posD == posN =
      if(not (hasPiece game posD))
         then [RemovePiece posE, MovePiece posO posD]
         else []
    | Just (_, _) <- getPieceAt game posN = []
    | otherwise = kingsCapture game _player posO posN posD posE
    where posN = ((fst posA + signum (fst posD - fst posO)), snd posA + signum (snd posD - snd posO))

-- | Implementation of PlayableGame class
instance PlayableGame DraughtsGame Int Tile Player Piece where
  curPlayer (DraughtsGame _ game) = curPlayer' game
  allPieces (DraughtsGame _ game) = boardPieces' game
  allPos (DraughtsGame _ game) = boardPos game

  moveEnabled _  = True

  -- | If this piece could be moved by this player
  canMove (DraughtsGame _ game) player position =
   case (getPieceAt game position) of
     Just (player2, _) -> player == player2
     otherwise -> False

  canMoveTo _ _ _ _ = True

  -- | Checking correctness of move
  move (DraughtsGame last game) _player posO posD
    | last /= None && Captured posO /= last = []
    | Just (_, King) <- getPieceAt game posO
    = if(abs (fst posO - fst posD) == abs (snd posO - snd posD))
        then kingsMove (DraughtsGame last game) _player posO posO posD
        else []
    | hasPiece game posO && not (hasPiece game posD) && forward && not (canAnyCapture (DraughtsGame last game))
    = if((snd posD == 1 && _player == Black) || (snd posD == 8 && _player == White))
        then [MovePiece posO posD, RemovePiece posD, AddPiece posD _player King]
        else [MovePiece posO posD]
    | hasPiece game posO && not (hasPiece game posD) && capture && enemy
    = if((snd posD == 1 && _player == Black) || (snd posD == 8 && _player == White)) && not (canCapture (fst posD, snd posD,_player,Man) game)
        then [MovePiece posO posD, RemovePiece posD, AddPiece posD _player King, RemovePiece posM]
        else [MovePiece posO posD, RemovePiece posM]
    | otherwise
    = []
   where forward = abs(fst posO - fst posD) == 1
           && ((_player == Black && (snd posO - snd posD == 1))  || (_player == White && (snd posO - snd posD == -1)))
           && abs(fst posO -fst posD) == 1
         capture = abs(fst posO - fst posD) == 2 && abs(snd posO - snd posD) == 2
         posM = ((fst posO + fst posD) `div` 2, (snd posO + snd posD) `div` 2)
         enemy = case (getPieceAt game posM) of
           Just (player, _) -> if player /= _player then True else False
           otherwise -> False

  -- | Move piece with persistence, setting LastMove and current player
  applyChange dg@(DraughtsGame last game) (MovePiece posO posD)
    | Just (player, piece) <- getPieceAt game posO
    -- x<0 -> there was captuing xD
    = if((canCapture (fst posO, snd posO, player, piece) game) && (canCapture (fst posD, snd posD,player,piece) game))
        then applyChanges (DraughtsGame (Captured posD) game) [RemovePiece posO, RemovePiece posD, AddPiece posD player piece]
        else applyChanges (togglePlayer dg) [RemovePiece posO, RemovePiece posD, AddPiece posD player piece]
    | otherwise = dg

  -- | Add piece of given player and piece type to position
  applyChange (DraughtsGame last game) (AddPiece (x,y) player piece )
    = DraughtsGame last (game { boardPieces' = (x,y,player,piece) : boardPieces' game })

  -- | Remove piece from position
  applyChange (DraughtsGame last game) (RemovePiece (x,y))
    = DraughtsGame last (game { boardPieces' = [ (x',y',player,piece)
                                              | (x',y',player,piece) <- boardPieces' game
                                              , (x /= x' || y /= y')]})
