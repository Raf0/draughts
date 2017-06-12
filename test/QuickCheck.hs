{-# LANGUAGE TemplateHaskell #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit hiding (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Draughts
import Game.Board.BasicTurnGame

main :: IO ()
main = defaultMain [testCanMove]

testCanMove :: Test
testCanMove = testGroup "Testing canMove function"
    [testCase "Testing white can move white" (
        assertEqual "Should white could move white." True (canMove (defaultDraughtsGame) White (1,2))),
     testCase "Testing black can move black" (
        assertEqual "Should black could move black." True (canMove (defaultDraughtsGame) Black (7,8))),
     testCase "Testing white can't move black" (
        assertEqual "Should white couldn't move black." False (canMove (defaultDraughtsGame) White (7,8))),
     testCase "Testing black can't move white" (
        assertEqual "Should black couldn't move white." False (canMove (defaultDraughtsGame) Black (1,2))),
    testCase "Testing white can't move nothing" (
        assertEqual "Should white couldn't move nothing." False (canMove (defaultDraughtsGame) White (4,5))),
    testCase "Testing black can't move nothing" (
        assertEqual "Should white couldn't move nothing." False (canMove (defaultDraughtsGame) Black (5,4)))]


-- canAnyCapture, canCapture, canCaptureThis - QuickCheck ?