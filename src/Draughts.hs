{-# LANGUAGE MultiParamTypeClasses #-}
module Draughts where

import Game.Board.BasicTurnGame

data Piece    = Man | King deriving(Eq)
data Tile     = TileBlack | TileWhite deriving(Eq)
data Player   = Black | White deriving (Eq)
data LastMove = Captured (Int,Int) | None deriving(Eq)

data DraughtsGame = DraughtsGame LastMove (GameState Int Tile Player Piece)


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

togglePlayer (DraughtsGame _ (GameState Black boardPos boardPieces')) = (DraughtsGame None (GameState White boardPos boardPieces'))
togglePlayer (DraughtsGame _ (GameState White boardPos boardPieces')) = (DraughtsGame None (GameState Black boardPos boardPieces'))


canAnyCapture (DraughtsGame _ game) = or (map (\(x,y,player,piece) ->
    if(player == curPlayer' game) then canCapture (x,y,player,piece) game
    else False) (boardPieces' game))

canCapture piece game =
    or (map (\p -> canCaptureThis piece p game) (boardPieces' game))

canCaptureThis (x,y,player,piece) (x2,y2,player2,piece2) game =
    if(player==player2 || x2==1 || x2==8 || y2==1 || y2==8) then False
    else abs (x -x2) == 1 && abs(y - y2) == 1 &&  not(hasPiece game (2*x2-x, 2*y2-y))

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


kingsCapture game _player posO posA posD posE
    | posD == posN =
      if(not (hasPiece game posD))
         then [RemovePiece posE, MovePiece posO posD]
         else []
    | Just (_, _) <- getPieceAt game posN = []
    | otherwise = kingsCapture game _player posO posN posD posE
    where posN = ((fst posA + signum (fst posD - fst posO)), snd posA + signum (snd posD - snd posO))


instance PlayableGame DraughtsGame Int Tile Player Piece where

  -- "Static" game view
  curPlayer (DraughtsGame _ game) = curPlayer' game
  allPieces (DraughtsGame _ game) = boardPieces' game
  allPos (DraughtsGame _ game) = boardPos game


  moveEnabled _  = True

  canMove (DraughtsGame _ game) player position =
   case (getPieceAt game position) of
     Just (player2, _) -> player == player2
     otherwise -> False

  canMoveTo _ _ _ _ = True

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
        then [RemovePiece posM, MovePiece posO posD, RemovePiece posD, AddPiece posD _player King]
        else [RemovePiece posM, MovePiece posO posD]
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


  applyChange dg@(DraughtsGame last game) (MovePiece posO posD)
    | Just (player, piece) <- getPieceAt game posO
    -- x<0 -> there was captuing xD
    = if(abs(fst posO - fst posD) > 1 && (canCapture (fst posD, snd posD,player,piece) game))
        then applyChanges (DraughtsGame (Captured posD) game) [RemovePiece posO, RemovePiece posD, AddPiece posD player piece]
        else applyChanges (togglePlayer dg) [RemovePiece posO, RemovePiece posD, AddPiece posD player piece]
    | otherwise = dg

  applyChange (DraughtsGame last game) (AddPiece (x,y) player piece )
    = DraughtsGame last (game { boardPieces' = (x,y,player,piece) : boardPieces' game })

  applyChange (DraughtsGame last game) (RemovePiece (x,y))
    = DraughtsGame last (game { boardPieces' = [ (x',y',player,piece)
                                              | (x',y',player,piece) <- boardPieces' game
                                              , (x /= x' || y /= y')]})
