{-# LANGUAGE RecordWildCards #-}

module Layout where

import Window

data WindowWithProperties
  = WindowWithProperties {
    windowId :: Window,
    position :: (Int, Int),
    size :: (Int, Int)
  }
  deriving (Eq, Show)

activateProperties :: WindowWithProperties -> IO ()
activateProperties WindowWithProperties{..} = do
  setWindowSize windowId size
  setWindowPosition windowId position

layout :: (Int, Int) -> Desktop -> [WindowWithProperties]
layout (width, height) desktop = case windows desktop of
  [] -> []
  active : inactive ->
    WindowWithProperties active (0, 0) (activeWidth, height) :
    map layoutInactive (zip [0..] inactive) ++
    []
    where
      activeWidth = round (fromIntegral width * 0.7)
      inactiveHeight =
        round (fromIntegral height / fromIntegral (length inactive))
      layoutInactive (i, window) =
        WindowWithProperties window
          (activeWidth, i * inactiveHeight)
          (width - activeWidth, inactiveHeight)
