{-# Language MultiParamTypeClasses, FunctionalDependencies #-}

import Control.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe
import Graphics.UI.Gtk
import Graphics.UI.Gtk.Layout.BackgroundContainer
import Graphics.UI.Gtk.Board.BoardLink
import Graphics.UI.Gtk.Board.TiledBoard
import GtkDraughts
import Draughts

-- | Starts draughts game
main :: IO ()
main = do
  -- View

  -- Initialise Gtk
  _ <- initGUI

  -- Create interface
  window  <- windowNew
  -- vbox    <- vBoxNew False 2
  bgBin   <- backgroundContainerNewWithPicture "img\\background.jpg"
  align   <- alignmentNew 0.5 0.5 0 0

  game    <- gtkGame
  board   <- attachGameRules game

  containerAdd align board
  containerAdd bgBin align
  containerAdd window bgBin

  widgetSetSizeRequest window 1400 900

  -- Close program if window is closed
  _ <- window `on` deleteEvent $ liftIO mainQuit >> return False

  -- Launch program with the main window
  widgetShowAll window
  mainGUI
