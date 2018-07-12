{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Window where

import Development.Shake
import Control.Arrow
import Data.Foldable

data Desktop
  = Desktop {
    desktop :: Int,
    windows :: [Window]
  }
  deriving Show

getCurrentDesktop :: IO Desktop
getCurrentDesktop = do
  Stdout (read -> desktop :: Int) <- cmd "xdotool get_desktop"
  Stdout (lines >>> map read -> windows :: [Int]) <-
    cmd "xdotool search --desktop" (show desktop) "."
  return $ Desktop desktop (map Window windows)

data Window
  = Window Int
  deriving Show

setWindowSize :: Window -> (Int, Int) -> IO ()
setWindowSize (Window id) (width, height) = do
  unit $ cmd "xdotool windowsize" (show id)
    (show width) (show height)

setWindowPosition :: Window -> (Int, Int) -> IO ()
setWindowPosition (Window id) (x, y) = do
  unit $ cmd "xdotool windowmove" (show id) (show x) (show y)
