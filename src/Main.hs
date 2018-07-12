{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Development.Shake
import Control.Arrow
import Data.Foldable
import Window
import Layout

main :: IO ()
main = do
  desktop <- getCurrentDesktop
  size <- getScreenSize
  mapM_ activateProperties $ layout size desktop

getScreenSize :: IO (Int, Int)
getScreenSize = do
  Stdout output <- cmd "xrandr"
  let "current" : width : "x" : height : _ =
        dropWhile (/= "current") $
        words $
        filter (/= ',') $
        output
  return (read width, read height)
