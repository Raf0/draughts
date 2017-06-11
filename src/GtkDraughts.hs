{-# LANGUAGE MultiParamTypeClasses #-}

module GtkDraughts  where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Board.TiledBoard
import Graphics.UI.Gtk.Board.BoardLink
import Draughts

gtkGame :: IO (Game DraughtsGame Int Tile Player Piece)
gtkGame = do
  -- The images used for tiles and pegs
  tileWhite  <- pixbufNewFromFile "img\\tile_white.png"
  tileBlack  <- pixbufNewFromFile "img\\tile_black.png"
  black <- pixbufNewFromFile "img\\man_black.png"
  white <- pixbufNewFromFile "img\\man_white.png"

  let game = Game visualAspects defaultDraughtsGame
      visualAspects = VisualGameAspects { tileF   = \tile -> if(tile==TileWhite) then tileWhite else tileBlack
                                        , pieceF  = \(player,_) -> if(player==Black)then black else white
                                        , bgColor = (65000, 50000, 50000)
                                        , bg      = Just (tileWhite, SizeAdjustment)
                                        }

  return game
