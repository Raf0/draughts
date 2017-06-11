{-# LANGUAGE MultiParamTypeClasses #-}
module Draughts where

import Game.Board.BasicTurnGame

data Piece    = Man | King deriving(Eq)
data Tile     = TileBlack | TileWhite deriving(Eq)
data Player   = Black | White deriving (Eq)

newtype DraughtsGame = DraughtsGame (GameState Int Tile Player Piece)


defaultDraughtsGame :: DraughtsGame
defaultDraughtsGame = DraughtsGame $ GameState
 { curPlayer'      = White
 , boardPos        = allTiles
 , boardPieces'    = pieces
 }
 where allTiles = [(x,y,TileWhite) | x <- [1..8], y <- [1..8], (x `mod` 2 == y `mod` 2)] ++
         [(x,y,TileBlack) | x <- [1..8], y <- [1..8], (x `mod` 2 /= y `mod` 2)]
       pieces   = [(x,y,Black,Man) | x <- [1..8], y<-[6..8], (x `mod` 2 == 0 && y==7) || (x `mod` 2 == 1 && y/=7) ] ++
         [(x,y,White,Man) | x <- [1..8], y<-[1..3], (x `mod` 2 == 1 && y==2) || (x `mod` 2 == 0 && y/=2) ]

togglePlayer (DraughtsGame (GameState Black boardPos boardPieces')) = (DraughtsGame (GameState White boardPos boardPieces'))
togglePlayer (DraughtsGame (GameState White boardPos boardPieces')) = (DraughtsGame (GameState Black boardPos boardPieces'))

instance PlayableGame DraughtsGame Int Tile Player Piece where

  -- "Static" game view
  curPlayer (DraughtsGame game) = curPlayer' game
  allPieces (DraughtsGame game) = boardPieces' game
  allPos (DraughtsGame game) = boardPos game


  moveEnabled _  = True

  canMove (DraughtsGame game) player position    =
   case (getPieceAt game position) of
     Just (player2, _) -> player == player2
     otherwise -> False

  canMoveTo _ _ _ _ = True

  move (DraughtsGame game) _player posO posD
    | hasPiece game posO && not (hasPiece game posD) && forward
    = [ MovePiece posO posD]
    | hasPiece game posO && not (hasPiece game posD) && capture && enemy
    = [ MovePiece posO posD, RemovePiece posM]
    | otherwise
    = []
   where forward = abs(fst posO - fst posD) == 1
           && (_player == Black && (snd posO - snd posD == 1)) || (_player == White && (snd posO - snd posD == -1))
         capture = abs(fst posO - fst posD) == 2 && abs(snd posO - snd posD) == 2
         posM = ((fst posO + fst posD) `div` 2, (snd posO + snd posD) `div` 2)
         enemy = case (getPieceAt game posM) of
           Just (player, _) -> if player /= _player then True else False
           otherwise -> False



  applyChange psg@(DraughtsGame game) (MovePiece posO posD)
    | Just (player, piece) <- getPieceAt game posO
    = applyChanges (togglePlayer psg) [RemovePiece posO, RemovePiece posD, AddPiece posD player piece]

    | otherwise = psg
  applyChange (DraughtsGame game) (AddPiece (x,y) player piece )
    = DraughtsGame (game { boardPieces' = (x,y,player,piece) : boardPieces' game })
  applyChange (DraughtsGame game) (RemovePiece (x,y))
    = DraughtsGame (game { boardPieces' = [ (x',y',player,piece)
                                              | (x',y',player,piece) <- boardPieces' game
                                              , (x /= x' || y /= y')]})
