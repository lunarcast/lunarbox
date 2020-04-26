module Test.Capability.Editor.Node.NodeInput where

import Prelude
import Data.List (List(..), reverse, (:))
import Lunarbox.Capability.Editor.Node.Arc (Arc(..), solveOverlaps)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: Spec Unit
spec =
  describe "The solveOverlaps function" do
    it "should return an empty matrix when given an empty list" do
      -- the type signature is needed to guarantee purescript a Show instance exists
      solveOverlaps (mempty :: _ (Arc String)) `shouldEqual` pure mempty
    it "should return a matrix with 1 element when giving 1 arc" do
      let
        arc = Arc 0.0 100.0 "my-arc"
      solveOverlaps (pure arc) `shouldEqual` (pure $ pure arc)
    it "should do nothing if there's no overlap" do
      let
        arc = Arc 0.0 100.0 1

        arc' = Arc 150.0 200.0 2

        layer = arc : arc' : Nil
      solveOverlaps layer `shouldEqual` pure layer
    it "should add the overlapping arc on a new layer" do
      let
        arc = Arc 0.0 100.0 1

        arc' = Arc 50.0 75.0 2

        layer = arc : arc' : Nil
      solveOverlaps layer `shouldEqual` (pure <$> layer)
      -- also do it in reverse
      solveOverlaps (reverse layer) `shouldEqual` (pure <$> (reverse $ layer))
    it "should detect overlaps on angles going over 360" do
      let
        arc = Arc 0.0 100.0 1

        arc' = Arc 300.0 50.0 2

        layer = arc : arc' : Nil
      solveOverlaps layer `shouldEqual` (pure <$> layer)
      -- also do it in reverse
      solveOverlaps (reverse layer) `shouldEqual` (pure <$> (reverse $ layer))
    it "should split multiple overlaps on multiple layers" do
      let
        arc = Arc 0.0 150.0 1

        arc' = Arc 50.0 200.0 2

        arc'' = Arc 100.0 250.0 3

        layer = arc : arc' : arc'' : Nil
      solveOverlaps layer `shouldEqual` (pure <$> layer)
      -- also do it in reverse
      solveOverlaps (reverse layer) `shouldEqual` (pure <$> (reverse $ layer))
