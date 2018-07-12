{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Development.Shake
import Control.Arrow
import Data.Foldable

main :: IO ()
main = do
  Stdout (read -> desktop :: Int) <- cmd "xdotool get_desktop"
  Stdout (lines >>> map read -> windows :: [Int]) <-
    cmd "xdotool search --desktop" (show desktop) "."
  size@(width, height) <- getScreenSize
  print size
  forM_ (zip windows [0 ..]) $ \ (window, i) -> do
    print window
    let windowWidth = 300
    unit $ cmd "xdotool windowsize" (show window)
      (show windowWidth) (show height)
    let x = i * windowWidth
    unit $ cmd "xdotool windowmove" (show window) (show x) "0"

getScreenSize :: IO (Int, Int)
getScreenSize = do
  Stdout output <- cmd "xrandr"
  let "current" : width : "x" : height : _ =
        dropWhile (/= "current") $
        words $
        filter (/= ',') $
        output
  return (read width, read height)
