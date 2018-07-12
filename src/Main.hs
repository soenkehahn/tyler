{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Development.Shake
import Control.Arrow
import Data.Foldable

main :: IO ()
main = do
  desktop <- getCurrentDesktop
  size@(width, height) <- getScreenSize
  forM_ (zip (windows desktop) [0 ..]) $ \ (window, i) -> do
    let windowWidth = 300
    setWindowSize window (windowWidth, height)
    let x = i * windowWidth
    setWindowPosition window (x, 0)

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

getScreenSize :: IO (Int, Int)
getScreenSize = do
  Stdout output <- cmd "xrandr"
  let "current" : width : "x" : height : _ =
        dropWhile (/= "current") $
        words $
        filter (/= ',') $
        output
  return (read width, read height)
