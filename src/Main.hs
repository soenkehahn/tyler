{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Development.Shake
import Control.Arrow
import Data.Foldable
import Window

main :: IO ()
main = do
  desktop <- getCurrentDesktop
  size@(width, height) <- getScreenSize
  forM_ (zip (windows desktop) [0 ..]) $ \ (window, i) -> do
    let windowWidth = 300
    setWindowSize window (windowWidth, height)
    let x = i * windowWidth
    setWindowPosition window (x, 0)

getScreenSize :: IO (Int, Int)
getScreenSize = do
  Stdout output <- cmd "xrandr"
  let "current" : width : "x" : height : _ =
        dropWhile (/= "current") $
        words $
        filter (/= ',') $
        output
  return (read width, read height)
