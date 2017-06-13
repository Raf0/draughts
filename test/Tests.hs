{-# LANGUAGE TemplateHaskell #-}
import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.QuickCheck
import Test.HUnit hiding (Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Draughts
import Game.Board.BasicTurnGame
import Control.Monad
import Data.List

main :: IO ()
main = defaultMain [testCanMove, testTooglePlayer, quickCheckTests]

game = defaultDraughtsGame

testCanMove :: Test
testCanMove = testGroup "Testing canMove function"
    [testCase "Testing white can move white" (
        assertEqual "Should white could move white." True (canMove (game) White (1,2))),
     testCase "Testing black can move black" (
        assertEqual "Should black could move black." True (canMove (game) Black (7,8))),
     testCase "Testing white can't move black" (
        assertEqual "Should white couldn't move black." False (canMove (game) White (7,8))),
     testCase "Testing black can't move white" (
        assertEqual "Should black couldn't move white." False (canMove (game) Black (1,2))),
    testCase "Testing white can't move nothing" (
        assertEqual "Should white couldn't move nothing." False (canMove (game) White (4,5))),
    testCase "Testing black can't move nothing" (
        assertEqual "Should white couldn't move nothing." False (canMove (game) Black (5,4)))]

testTooglePlayer :: Test
testTooglePlayer = testGroup "Testing tooglePlayer function"
    [testCase "Testing toogle White player" (
        assertEqual "Should toogle White to Black." Black (curPlayer . togglePlayer $ (game))),
     testCase "Testing toogle White player two times" (
        assertEqual "Should toogle White to White." White (curPlayer . togglePlayer . togglePlayer $ (game)))]

quickCheckTests :: Test
quickCheckTests = testGroup "Testing with QuickCheck"
    [testProperty "Testing MovePiece" prop_MovePiece,
    testProperty "Testing RemovePiece and AddPiece" prop_RemoveAddPiece]

prop_MovePiece =
    forAll generatePositions $ \(posO, posD) ->
       ((allPieces (applyChange game (MovePiece posO posD))) == (allPieces game)) ||
       (( (sortPieces $ allPieces (applyChange (applyChange game (MovePiece posO posD)) (MovePiece posD posO))) == (sortPieces $ allPieces game)))

prop_RemoveAddPiece =
    forAll generatePosition $ \pos -> case (getPieceAt (gameState game) pos) of
        Just(player,piece) ->
            ((sortPieces $ allPieces (applyChange (applyChange game (RemovePiece pos)) (AddPiece pos player piece))) == (sortPieces $ allPieces game))
        _ -> ((allPieces (applyChange game (RemovePiece pos))) == (allPieces game))

generatePosition :: Gen(Int,Int)
generatePosition = liftM2 (,) (choose (1,8)) (choose (1,8))

generatePositions :: Gen((Int, Int),(Int, Int))
generatePositions =
    liftM2 (,) generatePosition generatePosition

sortPieces = (sortBy (\(x,y,_,_) (x2,y2,_,_) ->
    if x>x2 then GT
    else if x<x2 then LT
        else if y>y2 then GT
            else LT))
