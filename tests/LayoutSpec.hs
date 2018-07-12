
module LayoutSpec where

import Test.Hspec
import Window
import Layout
import Data.List

spec :: Spec
spec = do
  describe "foo" $ do
    it "does not choke on empty desktops" $ do
      layout (1000, 500) (Desktop 0 []) `shouldBe` []

    context "when there's one active window" $ do
      let firstWindow = layout (1000, 500) (Desktop 0 [Window 23]) !! 0

      it "positions the active window at (0, 0)" $ do
        windowId firstWindow `shouldBe` Window 23
        position firstWindow `shouldBe` (0, 0)

      it "gives the active window 70% of the screen width" $ do
        fst (size firstWindow) `shouldBe` 700

      it "gives the active window 100% off the screen height" $ do
        snd (size firstWindow) `shouldBe` 500

    context "when there's inactive windows" $ do
      let windows = [Window 23, Window 42, Window 51]
          inactiveWindows = drop 1 $ layout (1000, 500) (Desktop 0 windows)
      it "positions inactive windows right of the active window" $ do
        nub (map (fst . position) inactiveWindows) `shouldBe` [700]

      it "positions inactive windows from top to bottom" $ do
        map windowId inactiveWindows `shouldBe` map Window [42, 51]
        map (snd . position) inactiveWindows `shouldBe` [0, 250]

      it "sets the inactive window width to 30% of the screen width" $ do
        nub (map (fst . size) inactiveWindows) `shouldBe` [300]

      it "sets the inactive window height to fill the screen height" $ do
        nub (map (snd . size) inactiveWindows) `shouldBe` [250]
