-- #############################################################################
-- ####### YOUR UNIT TESTS                                           ###########
-- ####### Note: execute tests using "stack test deathstacks:units"  ###########
-- #############################################################################
import Test.Hspec
import Board
    ( buildBoard,
      validateFEN,
      Cell(Empty,Stack),
      Player(Red, Blue),
      path, Pos (..), Dir (..))
import Deathstacks
    ( Move(Move),
      isValidMove,
      possibleMoves,
      playerWon )

main :: IO ()
-- Fast alle Test Cases von CoPilot geschrieben
main = hspec $ do
    describe "validateFEN function tests" $ do
        it "Test 1: 'rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb' should be valid" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` True

        it "Test 2: 'rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb' should be valid" $ do
            validateFEN "rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` True

        it "Test 3: ',,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,' should be valid" $ do
            validateFEN ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` True

        it "Test 4: '/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,' should be invalid" $ do
            validateFEN "/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False

        it "Test 5: '/rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb' (leading slash) should be invalid" $ do
            validateFEN "/rr,,,,,rr/,,,,,/,bbr,rr,,rrb,/,,,,,/,,,,,/bb,bb,,,bbrrrb,bb" `shouldBe` False

        it "Test 6: 'rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb/' (trailing slash) should be invalid" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb/" `shouldBe` False

        it "Test 7: '/rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb/' (leading and trailing slash) should be invalid" $ do
            validateFEN "/rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb/" `shouldBe` False

        it "Test 8: 'rrrrr,rrrrr,rrrrr,rrrrr,rrrrr,rrrrr/,rrrrr,rrrrr,rrrrr,rrrrr,rrrrr/,bbbbb,bbbbb,bbbbb,bbbbb,bbbbb/,bbbbb,bbbbb,bbbbb,bbbbb,bbbbb/,rrrbrbrb,rrrbrbrb,rrrbrbrb,rrrbrbrb,rrrbrbrb/,,,,,' should be valid" $ do
            validateFEN "rrrrr,rrrrr,rrrrr,rrrrr,rrrrr,rrrrr/,rrrrr,rrrrr,rrrrr,rrrrr,rrrrr/,bbbbb,bbbbb,bbbbb,bbbbb,bbbbb/,bbbbb,bbbbb,bbbbb,bbbbb,bbbbb/,rrrbrbrb,rrrbrbrb,rrrbrbrb,rrrbrbrb,rrrbrbrb/,,,,," `shouldBe` True

        it "Test 9: 'a,b,c,d,e,f/g,h,i,j,k,l/m,n,o,p,q,r/s,t,u,v,w,x/y,z,1,2,3,4/5,6,7,8,9,0' (invalid characters) should be invalid" $ do
            validateFEN "/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,," `shouldBe` False

        it "Test 10: '' (empty string) should be invalid" $ do
            validateFEN "" `shouldBe` False

        it "Test 11: 'r' should be invalid" $ do
            validateFEN "r" `shouldBe` False

        it "Test 12: 'rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb/,,,,,/,,,,,' (7 rows) should be invalid" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb/,,,,,/,,,,," `shouldBe` False

        it "Test 13: 'rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb' (5 rows) should be invalid" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb" `shouldBe` False

        it "Test 14: 'rr,rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb' (7 cells in a row) should be invalid" $ do
            validateFEN "rr,rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False

        it "Test 15: 'rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb' (5 cells in a row) should be invalid" $ do
            validateFEN "rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False

        it "Test 16: ' rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb ' (leading and trailing spaces) should be invalid" $ do
            validateFEN " rr,rr,rr,rr,rr,rr/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb " `shouldBe` False

        it "Test 17: ',/,/,/,/,/' (wrong format) should be invalid" $ do
            validateFEN ",/,/,/,/,/," `shouldBe` False

        it "Test 18: 'rr,rr,rr,rr,rr,rr,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb' (extra comma in a row) should be invalid" $ do
            validateFEN "rr,rr,rr,rr,rr,rr,/,,,,,/,,,,,/,,,,,/bb,bb,bb,bb,bb,bb" `shouldBe` False

        it "Test 19: 'rr,rr,rr,rr,rr,rr/rr,rr,rr,rr,rr,rr/rr,rr,rr,rr,rr,rr/rr,rr,rr,rr,rr,rr/rr,rr,rr,rr,rr,rr/rr,rr,rr,rr,rr,rr' should be valid" $ do
            validateFEN "rr,rr,rr,rr,rr,rr/rr,rr,rr,rr,rr,rr/rr,rr,rr,rr,rr,rr/rr,rr,rr,rr,rr,rr/rr,rr,rr,rr,rr,rr/rr,rr,rr,rr,rr,rr" `shouldBe` True

        it "Test 20: ',,,,,,,,,,' (wrong format) should be invalid" $ do
            validateFEN ",/,/,/,/,/," `shouldBe` False

    describe "buildBoard" $ do
        it "Test 1: builds an empty board" $ do
            let fen = ",,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,"
            let expectedBoard = replicate 6 (replicate 6 Empty)
            buildBoard fen `shouldBe` expectedBoard

        it "Test 2: builds a board with one red cell" $ do
            let fen = "r,,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,"
            let expectedBoard = (Stack [Red] : replicate 5 Empty) : replicate 5 (replicate 6 Empty)
            buildBoard fen `shouldBe` expectedBoard

        it "Test 3: builds a board with one blue cell" $ do
            let fen = ",b,,,,/,,,,,/,,,,,/,,,,,/,,,,,/,,,,,"
            let expectedBoard = (Empty : Stack [Blue] : replicate 4 Empty) : replicate 5 (replicate 6 Empty)
            buildBoard fen `shouldBe` expectedBoard

        it "Test 4: builds a full board" $ do
            let fen = "r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b"
            let expectedBoard = replicate 6 (concat $ replicate 3 [Stack [Red], Stack [Blue]])
            buildBoard fen `shouldBe` expectedBoard

        it "Test 5: builds a board with alternating red and blue cells in each row" $ do
            let fen = "r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b"
            let expectedBoard = replicate 6 (concat $ replicate 3 [Stack [Red], Stack [Blue]])
            buildBoard fen `shouldBe` expectedBoard

        it "Test 6: builds a board with alternating red and blue cells in each column" $ do
            let fen = "r,r,r,r,r,r/b,b,b,b,b,b/r,r,r,r,r,r/b,b,b,b,b,b/r,r,r,r,r,r/b,b,b,b,b,b"
            let expectedBoard = concat $ replicate 3 [replicate 6 (Stack [Red]), replicate 6 (Stack [Blue])]
            buildBoard fen `shouldBe` expectedBoard

        it "Test 7: builds a board with a diagonal of red cells" $ do
            let fen = "r,,,,,/b,r,,,,/b,b,r,,,/b,b,b,r,,/b,b,b,b,r,/b,b,b,b,b,r"
            let expectedBoard = [Stack [Red] : replicate 5 Empty,
                                 Stack [Blue] : Stack [Red] : replicate 4 Empty,
                                 replicate 2 (Stack [Blue]) ++ (Stack [Red] : replicate 3 Empty),
                                 replicate 3 (Stack [Blue]) ++ (Stack [Red] : replicate 2 Empty),
                                 replicate 4 (Stack [Blue]) ++ [Stack [Red], Empty],
                                 replicate 5 (Stack [Blue]) ++ [Stack [Red]]]
            buildBoard fen `shouldBe` expectedBoard

        it "Test 8: builds a board with a diagonal of blue cells" $ do
            let fen = "b,,,,,/r,b,,,,/r,r,b,,,/r,r,r,b,,/r,r,r,r,b,/r,r,r,r,r,b"
            let expectedBoard = [Stack [Blue] : replicate 5 Empty,
                                 Stack [Red] : Stack [Blue] : replicate 4 Empty,
                                 replicate 2 (Stack [Red]) ++ (Stack [Blue] : replicate 3 Empty),
                                 replicate 3 (Stack [Red]) ++ (Stack [Blue] : replicate 2 Empty),
                                 replicate 4 (Stack [Red]) ++ [Stack [Blue], Empty],
                                 replicate 5 (Stack [Red]) ++ [Stack [Blue]]]
            buildBoard fen `shouldBe` expectedBoard

        it "Test 9: builds a board with a diagonal of alternating red and blue cells" $ do
            let fen = "r,,,,,/b,r,,,,/r,b,r,,,/b,r,b,r,,/r,b,r,b,r,/b,r,b,r,b,r"
            let expectedBoard = [Stack [Red] : replicate 5 Empty,
                         Stack [Blue] : Stack [Red] : replicate 4 Empty,
                         Stack [Red] : Stack [Blue] : Stack [Red] : replicate 3 Empty,
                         Stack [Blue] : Stack [Red] : Stack [Blue] : Stack [Red] : replicate 2 Empty,
                         Stack [Red] : Stack [Blue] : Stack [Red] : Stack [Blue] : Stack [Red] : replicate 1 Empty,
                         [Stack [Blue], Stack [Red], Stack [Blue], Stack [Red], Stack [Blue], Stack [Red]]]
            buildBoard fen `shouldBe` expectedBoard

        it "Test 10: only red cells" $ do
            let fen = "r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r"
            let expectedBoard = replicate 6 (replicate 6 (Stack [Red]))
            buildBoard fen `shouldBe` expectedBoard

        it "Test 11: only blue cells" $ do
            let fen = "b,b,b,b,b,b/b,b,b,b,b,b/b,b,b,b,b,b/b,b,b,b,b,b/b,b,b,b,b,b/b,b,b,b,b,b"
            let expectedBoard = replicate 6 (replicate 6 (Stack [Blue]))
            buildBoard fen `shouldBe` expectedBoard

        it "Test 12: String with stacks of sizes greater than 1" $ do
            let fen = "rr,rrr,rrrr,rrrrr,rrrrrr,rrrrrrr/rb,rbb,rbbb,rbbbb,rbbbbb,rbbbbbb/rb,rbr,rbrb,rbrbr,rbrbrb,rbrbrbr/br,brb,brbr,brbrb,brbrbr,brbrbrb/br,brr,brrr,brrrr,brrrrr,brrrrrr/bb,bbb,bbbb,bbbbb,bbbbbb,bbbbbbb"
            let expectedBoard = [[Stack (replicate 2 Red), Stack (replicate 3 Red), Stack (replicate 4 Red), Stack (replicate 5 Red), Stack (replicate 6 Red), Stack (replicate 7 Red)],
                     [Stack [Red, Blue], Stack [Red, Blue, Blue], Stack [Red, Blue, Blue, Blue], Stack [Red, Blue, Blue, Blue, Blue], Stack [Red, Blue, Blue, Blue, Blue, Blue], Stack [Red, Blue, Blue, Blue, Blue, Blue, Blue]],
                     [Stack [Red, Blue], Stack [Red, Blue, Red], Stack [Red, Blue, Red, Blue], Stack [Red, Blue, Red, Blue, Red], Stack [Red, Blue, Red, Blue, Red, Blue], Stack [Red, Blue, Red, Blue, Red, Blue, Red]],
                     [Stack [Blue, Red], Stack [Blue, Red, Blue], Stack [Blue, Red, Blue, Red], Stack [Blue, Red, Blue, Red, Blue], Stack [Blue, Red, Blue, Red, Blue, Red], Stack [Blue, Red, Blue, Red, Blue, Red, Blue]],
                     [Stack [Blue, Red], Stack [Blue, Red, Red], Stack [Blue, Red, Red, Red], Stack [Blue, Red, Red, Red, Red], Stack [Blue, Red, Red, Red, Red, Red], Stack [Blue, Red, Red, Red, Red, Red, Red]],
                     [Stack (replicate 2 Blue), Stack (replicate 3 Blue), Stack (replicate 4 Blue), Stack (replicate 5 Blue), Stack (replicate 6 Blue), Stack (replicate 7 Blue)]]
            buildBoard fen `shouldBe` expectedBoard

        describe "path" $ do
            it "Test 1: generates a path from a1 to the east for 2 steps" $
                path (Pos 'a' 1) East 2 `shouldBe` [Pos 'a' 1, Pos 'b' 1, Pos 'c' 1]

            it "Test 2: generates a path from a1 to the south for 2 steps (reflects off the edge)" $
                path (Pos 'a' 1) South 2 `shouldBe` [Pos 'a' 1, Pos 'a' 2, Pos 'a' 3]

            it "Test 3: generates a path from f6 to the west for 2 steps" $
                path (Pos 'f' 6) West 2 `shouldBe` [Pos 'f' 6, Pos 'e' 6, Pos 'd' 6]

            it "Test 4: generates a path from f6 to the north for 2 steps (reflects off the edge)" $
                path (Pos 'f' 6) North 2 `shouldBe` [Pos 'f' 6, Pos 'f' 5, Pos 'f' 4]

            it "Test 5: generates a path from a1 to the north for 2 steps" $
                path (Pos 'a' 1) North 2 `shouldBe` [Pos 'a' 1, Pos 'a' 2, Pos 'a' 3]

            it "Test 6: generates a path from a1 to the west for 2 steps (reflects off the edge)" $
                path (Pos 'a' 1) West 2 `shouldBe` [Pos 'a' 1, Pos 'b' 1, Pos 'c' 1]

            it "Test 7: generates a path from f6 to the south for 2 steps" $
                path (Pos 'f' 6) South 2 `shouldBe` [Pos 'f' 6, Pos 'f' 5, Pos 'f' 4]

            it "Test 8: generates a path from f6 to the east for 2 steps (reflects off the edge)" $
                path (Pos 'f' 6) East 2 `shouldBe` [Pos 'f' 6, Pos 'e' 6, Pos 'd' 6]

            it "Test 9: generates a path from a1 to the north east for 2 steps" $
                path (Pos 'a' 1) NorthEast 2 `shouldBe` [Pos 'a' 1, Pos 'b' 2, Pos 'c' 3]

            it "Test 10: generates a path from f6 to the south west for 2 steps" $
                path (Pos 'f' 6) SouthWest 2 `shouldBe` [Pos 'f' 6, Pos 'e' 5, Pos 'd' 4]

            it "Test 11: generates a path from a6 to the south east for 2 steps" $
                path (Pos 'a' 6) SouthEast 2 `shouldBe` [Pos 'a' 6, Pos 'b' 5, Pos 'c' 4]

            it "Test 12: generates a path from f1 to the north west for 2 steps" $
                path (Pos 'f' 1) NorthWest 2 `shouldBe` [Pos 'f' 1, Pos 'e' 2, Pos 'd' 3]

            it "Test 13: generates a path from f1 to the south west for 2 steps (reflects off the edge)" $
                path (Pos 'f' 1) SouthWest 2 `shouldBe` [Pos 'f' 1, Pos 'e' 2, Pos 'd' 3]

            it "Test 14: generates a path from f6 to the north west for 6 steps (reflects off the edge)" $
                path (Pos 'f' 6) NorthWest 6 `shouldBe` [Pos 'f' 6, Pos 'e' 5, Pos 'd' 4, Pos 'c' 3, Pos 'b' 2, Pos 'a' 1, Pos 'b' 2]

            it "Test 15: generates a path from a6 to the west for 6 steps (reflects off the edge)" $
                path (Pos 'a' 6) West 6 `shouldBe` [Pos 'a' 6, Pos 'b' 6, Pos 'c' 6, Pos 'd' 6, Pos 'e' 6, Pos 'f' 6, Pos 'e' 6]

            it "Test 16: generates a path from f1 to the north east for 6 steps (reflects off the edge)" $
                path (Pos 'f' 1) NorthEast 6 `shouldBe` [Pos 'f' 1, Pos 'e' 2, Pos 'd' 3, Pos 'c' 4, Pos 'b' 5, Pos 'a' 6, Pos 'b' 5]

            it "Test 17: generates a path from f6 to the north east for 6 steps (reflects off the edge)" $
                path (Pos 'f' 6) NorthEast 6 `shouldBe` [Pos 'f' 6, Pos 'e' 5, Pos 'd' 4, Pos 'c' 3, Pos 'b' 2, Pos 'a' 1, Pos 'b' 2]

            it "Test 18: generates a path from f1 to the south east for 6 steps (reflects off the edge)" $
                path (Pos 'f' 1) SouthEast 6 `shouldBe` [Pos 'f' 1, Pos 'e' 2, Pos 'd' 3, Pos 'c' 4, Pos 'b' 5, Pos 'a' 6, Pos 'b' 5]

            it "Test 19: generates a path from f4 to the south east for 6 steps (reflects off the edge)" $
                path (Pos 'f' 4) SouthEast 6 `shouldBe` [Pos 'f' 4, Pos 'e' 3, Pos 'd' 2, Pos 'c' 1, Pos 'b' 2, Pos 'a' 3, Pos 'b' 4]

            it "Test 20: generates a path from a1 to the south west for 6 steps (reflects off the edge)" $
                path (Pos 'a' 1) SouthWest 6 `shouldBe` [Pos 'a' 1, Pos 'b' 2, Pos 'c' 3, Pos 'd' 4, Pos 'e' 5, Pos 'f' 6, Pos 'e' 5]

            it "Test 21: generates a path from a4 to the south west for 6 steps (reflects off the edge)" $
                path (Pos 'a' 4) SouthWest 6 `shouldBe` [Pos 'a' 4, Pos 'b' 3, Pos 'c' 2, Pos 'd' 1, Pos 'e' 2, Pos 'f' 3, Pos 'e' 4]

            it "Test 22: generates a path from a6 to the north west for 6 steps (reflects off the edge)" $
                path (Pos 'a' 6) NorthWest 6 `shouldBe` [Pos 'a' 6, Pos 'b' 5, Pos 'c' 4, Pos 'd' 3, Pos 'e' 2, Pos 'f' 1, Pos 'e' 2]

            it "Test 23: generates a path from a4 to the north west for 6 steps (reflects off the edge)" $
                path (Pos 'a' 4) NorthWest 6 `shouldBe` [Pos 'a' 4, Pos 'b' 5, Pos 'c' 6, Pos 'd' 5, Pos 'e' 4, Pos 'f' 3, Pos 'e' 2]

            it "Test 23: generates a path from a4 to the north west for 6 steps (reflects off the edge)" $
                path (Pos 'a' 4) NorthWest 6 `shouldNotBe` [Pos 'a' 4, Pos 'b' 5, Pos 'd' 6, Pos 'e' 5, Pos 'f' 4, Pos 'a' 3, Pos 'b' 2]

        describe "playerWon" $ do
            it "returns Just Red when all stacks top with Red" $ do
                let board = replicate 6 (replicate 6 (Stack [Red]))
                playerWon board `shouldBe` Just Red

            it "returns Just Blue when all stacks top with Blue" $ do
                let board = replicate 6 (replicate 6 (Stack [Blue]))
                playerWon board `shouldBe` Just Blue

            it "returns Nothing when stacks top with different players" $ do
                let board = [Stack [Red], Stack [Blue]] : replicate 5 (replicate 6 Empty)
                playerWon board `shouldBe` Nothing

            it "return Nothing when Board is empty" $ do
                let board = replicate 6 (replicate 6 Empty)
                playerWon board `shouldBe` Nothing

            it "return Just Blue when Blue is on top of all Red Stacks" $ do
                let board = [ Stack [Blue, Red] : replicate 5 Empty,
                        [Empty, Stack [Blue, Red]] ++ replicate 4 Empty,
                        replicate 2 Empty ++ [Stack [Blue, Red]] ++ replicate 3 Empty,
                        replicate 3 Empty ++ [Stack [Blue, Red]] ++ replicate 2 Empty,
                        replicate 4 Empty ++ [Stack [Blue, Red]] ++ replicate 1 Empty,
                        replicate 5 Empty ++ [Stack [Blue, Red]] ]
                playerWon board `shouldBe` Just Blue

            it "return Just Red when Red is on top of all Blue Stacks" $ do
                let board = [ Stack [Red, Blue] : replicate 5 Empty,
                        [Empty, Stack [Red, Blue]] ++ replicate 4 Empty,
                        replicate 2 Empty ++ [Stack [Red, Blue]] ++ replicate 3 Empty,
                        replicate 3 Empty ++ [Stack [Red, Blue]] ++ replicate 2 Empty,
                        replicate 4 Empty ++ [Stack [Red, Blue]] ++ replicate 1 Empty,
                        replicate 5 Empty ++ [Stack [Red, Blue]] ]
                playerWon board `shouldBe` Just Red

            it "returns Just Blue when Blue is on top of all mixed stacks" $ do
                let board = [ Stack [Blue, Red, Blue, Red] : replicate 5 Empty,
                              replicate 3 Empty ++ [Stack [Blue, Red, Blue, Red]] ++ replicate 2 Empty,
                              replicate 5 Empty ++ [Stack [Blue, Red, Blue, Red]] ]
                playerWon board `shouldBe` Just Blue

            it "returns Just Red when Red is on top of all mixed stacks" $ do
                let board = [ Stack [Red, Blue, Red, Blue] : replicate 5 Empty,
                              replicate 3 Empty ++ [Stack [Red, Blue, Red, Blue]] ++ replicate 2 Empty,
                              replicate 5 Empty ++ [Stack [Red, Blue, Red, Blue]] ]
                playerWon board `shouldBe` Just Red

            it "returns Just Blue when Blue is on top of all mixed stacks" $ do
                let board = [ Stack [Blue, Red, Blue] : replicate 5 Empty,
                              replicate 2 Empty ++ [Stack [Blue, Red, Blue]] ++ replicate 3 Empty,
                              replicate 4 Empty ++ [Stack [Blue, Red, Blue]] ++ replicate 1 Empty,
                              replicate 5 Empty ++ [Stack [Blue, Red, Blue]] ]
                playerWon board `shouldBe` Just Blue

            it "returns Just Red when Red is on top of all mixed stacks" $ do
                let board = [ Stack [Red, Blue, Red] : replicate 5 Empty,
                              replicate 2 Empty ++ [Stack [Red, Blue, Red]] ++ replicate 3 Empty,
                              replicate 4 Empty ++ [Stack [Red, Blue, Red]] ++ replicate 1 Empty,
                              replicate 5 Empty ++ [Stack [Red, Blue, Red]] ]
                playerWon board `shouldBe` Just Red

            it "returns Nothing when there are different colors on top" $ do
                let board = [ Stack [Blue, Red, Blue] : replicate 5 Empty,
                              replicate 2 Empty ++ [Stack [Red, Blue, Red]] ++ replicate 3 Empty,
                              replicate 4 Empty ++ [Stack [Blue, Red, Blue]] ++ replicate 1 Empty,
                              replicate 5 Empty ++ [Stack [Red, Blue, Red]] ]
                playerWon board `shouldBe` Nothing

        describe "possibleMoves" $ do
            
            {- it "returns correct moves in list for two piece stack on c4" $ do
                possibleMoves (Pos 'c' 4) (Stack [Red, Blue]) `shouldMatchList` ([Move (Pos 'c' 4) (Pos 'c' 5) 1,Move (Pos 'c' 4) (Pos 'd' 5) 1,
                    Move (Pos 'c' 4) (Pos 'd' 4) 1,Move (Pos 'c' 4) (Pos 'd' 3) 1,Move (Pos 'c' 4) (Pos 'c' 3) 1,
                    Move (Pos 'c' 4) (Pos 'b' 3) 1,Move (Pos 'c' 4) (Pos 'b' 4) 1,Move (Pos 'c' 4) (Pos 'b' 5) 1, 
                    Move (Pos 'c' 4) (Pos 'c' 6) 2, Move (Pos 'c' 4) (Pos 'e' 6) 2,Move (Pos 'c' 4) (Pos 'e' 4) 2, Move (Pos 'c' 4) (Pos 'e' 2) 2, Move (Pos 'c' 4) (Pos 'c' 2) 2,
                    Move (Pos 'c' 4) (Pos 'a' 2) 2, Move (Pos 'c' 4) (Pos 'a' 4) 2, Move (Pos 'c' 4) (Pos 'a' 6) 2] :: [Move])

            it "returns correct moves in list for three piece stack on c4" $ do
                possibleMoves (Pos 'c' 4) (Stack [Blue, Red, Blue]) `shouldMatchList`
                    ([Move (Pos 'c' 4) (Pos 'c' 5) 1,Move (Pos 'c' 4) (Pos 'd' 5) 1,Move (Pos 'c' 4) (Pos 'd' 4) 1,Move (Pos 'c' 4) (Pos 'd' 3) 1,
                    Move (Pos 'c' 4) (Pos 'c' 3) 1, Move (Pos 'c' 4) (Pos 'b' 3) 1,Move (Pos 'c' 4) (Pos 'b' 4) 1,Move (Pos 'c' 4) (Pos 'b' 5) 1,
                    Move (Pos 'c' 4) (Pos 'c' 6) 2, Move (Pos 'c' 4) (Pos 'e' 6) 2,Move (Pos 'c' 4) (Pos 'e' 4) 2, Move (Pos 'c' 4) (Pos 'e' 2) 2,
                    Move (Pos 'c' 4) (Pos 'c' 2) 2, Move (Pos 'c' 4) (Pos 'a' 2) 2, Move (Pos 'c' 4) (Pos 'a' 4) 2, Move (Pos 'c' 4) (Pos 'a' 6) 2,
                    Move (Pos 'c' 4) (Pos 'c' 5) 3, Move (Pos 'c' 4) (Pos 'f' 5) 3,Move (Pos 'c' 4) (Pos 'f' 4) 3, Move (Pos 'c' 4) (Pos 'f' 1) 3,
                    Move (Pos 'c' 4) (Pos 'c' 1) 3, Move (Pos 'c' 4) (Pos 'b' 1) 3, Move (Pos 'c' 4) (Pos 'b' 4) 3, Move (Pos 'c' 4) (Pos 'b' 5) 3] :: [Move])

            it "returns correct moves in list for three piece stack on a1" $ do
                possibleMoves (Pos 'a' 1) (Stack [Red, Red]) `shouldMatchList`
                    [Move (Pos 'a' 1) (Pos 'a' 2) 1, Move (Pos 'a' 1) (Pos 'b' 2) 1, Move (Pos 'a' 1) (Pos 'b' 1) 1,  
                        Move (Pos 'a' 1) (Pos 'a' 3) 2, Move (Pos 'a' 1) (Pos 'c' 3) 2, Move (Pos 'a' 1) (Pos 'c' 1) 2]

            -}

            it "returns correct moves in list for five piece stack on f6" $ do
                    possibleMoves (Pos 'f' 6) (Stack [Red, Blue, Blue, Red, Blue]) `shouldMatchList` 
                            ([Move (Pos 'f' 6) (Pos 'f' 5) 1, Move (Pos 'f' 6) (Pos 'e' 5) 1, Move (Pos 'f' 6) (Pos 'e' 6) 1,
                                Move (Pos 'f' 6) (Pos 'f' 4) 2, Move (Pos 'f' 6) (Pos 'd' 4) 2, Move (Pos 'f' 6) (Pos 'd' 6) 2,
                                Move (Pos 'f' 6) (Pos 'f' 3) 3, Move (Pos 'f' 6) (Pos 'c' 3) 3, Move (Pos 'f' 6) (Pos 'c' 6) 3,
                                Move (Pos 'f' 6) (Pos 'f' 2) 4, Move (Pos 'f' 6) (Pos 'b' 2) 4, Move (Pos 'f' 6) (Pos 'b' 6) 4,
                                Move (Pos 'f' 6) (Pos 'f' 1) 5, Move (Pos 'f' 6) (Pos 'a' 1) 5, Move (Pos 'f' 6) (Pos 'a' 6) 5] :: [Move])

            it "returns correct moves in a list for five piece stack on a6" $ do
                possibleMoves (Pos 'a' 6) (Stack [Red, Blue , Blue, Red, Blue]) `shouldMatchList`
                    ([Move (Pos 'a' 6) (Pos 'a' 5) 1, Move (Pos 'a' 6) (Pos 'b' 5) 1, Move (Pos 'a' 6) (Pos 'b' 6) 1,
                        Move (Pos 'a' 6) (Pos 'a' 4) 2, Move (Pos 'a' 6) (Pos 'c' 4) 2, Move (Pos 'a' 6) (Pos 'c' 6) 2,
                        Move (Pos 'a' 6) (Pos 'a' 3) 3, Move (Pos 'a' 6) (Pos 'd' 3) 3, Move (Pos 'a' 6) (Pos 'd' 6) 3,
                        Move (Pos 'a' 6) (Pos 'a' 2) 4, Move (Pos 'a' 6) (Pos 'e' 2) 4, Move (Pos 'a' 6) (Pos 'e' 6) 4,
                        Move (Pos 'a' 6) (Pos 'a' 1) 5, Move (Pos 'a' 6) (Pos 'f' 1) 5, Move (Pos 'a' 6) (Pos 'f' 6) 5] :: [Move])

            it "returns correct moves in a list for five piece stack on f1" $ do
                possibleMoves (Pos 'f' 1) (Stack [Red, Blue, Blue, Red, Blue]) `shouldMatchList`
                    ([Move (Pos 'f' 1) (Pos 'f' 2) 1, Move (Pos 'f' 1) (Pos 'e' 2) 1, Move (Pos 'f' 1) (Pos 'e' 1) 1,
                        Move (Pos 'f' 1) (Pos 'd' 1) 2, Move (Pos 'f' 1) (Pos 'd' 3) 2, Move (Pos 'f' 1) (Pos 'f' 3) 2,
                        Move (Pos 'f' 1) (Pos 'c' 1) 3, Move (Pos 'f' 1) (Pos 'c' 4) 3, Move (Pos 'f' 1) (Pos 'f' 4) 3,
                        Move (Pos 'f' 1) (Pos 'b' 1) 4, Move (Pos 'f' 1) (Pos 'b' 5) 4, Move (Pos 'f' 1) (Pos 'f' 5) 4,
                        Move (Pos 'f' 1) (Pos 'a' 1) 5, Move (Pos 'f' 1) (Pos 'a' 6) 5, Move (Pos 'f' 1) (Pos 'f' 6) 5] :: [Move])

            it "returns correct moves in a list for five piece stack on a1" $ do
                possibleMoves (Pos 'a' 1) (Stack [Red, Blue, Blue, Red, Blue]) `shouldMatchList`
                    ([Move (Pos 'a' 1) (Pos 'a' 2) 1, Move (Pos 'a' 1) (Pos 'b' 2) 1, Move (Pos 'a' 1) (Pos 'b' 1) 1,
                        Move (Pos 'a' 1) (Pos 'a' 3) 2, Move (Pos 'a' 1) (Pos 'c' 3) 2, Move (Pos 'a' 1) (Pos 'c' 1) 2,
                        Move (Pos 'a' 1) (Pos 'a' 4) 3, Move (Pos 'a' 1) (Pos 'd' 4) 3, Move (Pos 'a' 1) (Pos 'd' 1) 3,
                        Move (Pos 'a' 1) (Pos 'a' 5) 4, Move (Pos 'a' 1) (Pos 'e' 5) 4, Move (Pos 'a' 1) (Pos 'e' 1) 4,
                        Move (Pos 'a' 1) (Pos 'a' 6) 5, Move (Pos 'a' 1) (Pos 'f' 6) 5, Move (Pos 'a' 1) (Pos 'f' 1) 5] :: [Move])

            it "returns Empty list for empty stack on a1" $ do
                possibleMoves (Pos 'a' 1) Empty `shouldBe` []

            it "returns correct list for moves with startPos == endPos" $ do
                possibleMoves (Pos 'e' 5) (Stack [Red, Blue]) `shouldMatchList`
                    ([Move (Pos 'e' 5) (Pos 'f' 6) 1, Move (Pos 'e' 5) (Pos 'e' 6) 1, Move (Pos 'e' 5) (Pos 'd' 6) 1, Move (Pos 'e' 5) (Pos 'd' 5) 1,
                    Move (Pos 'e' 5) (Pos 'f' 5) 1, Move (Pos 'e' 5) (Pos 'd' 4) 1, Move (Pos 'e' 5) (Pos 'e' 4) 1, Move (Pos 'e' 5) (Pos 'f' 4) 1,
                    Move (Pos 'e' 5) (Pos 'c' 5) 2, Move (Pos 'e' 5) (Pos 'e' 3) 2, Move (Pos 'e' 5) (Pos 'c' 3) 2] :: [Move])




 -- Eq und Pos Unit Tests da Validation sonst nicht 100% erreicht
{-        describe "Pos" $ do
            it "should correctly innit Pos values" $ do
                let position = Pos 'c' 4
                col position `shouldBe` 'c'
                row position `shouldBe` 4
        describe "Eq Pos" $ do
            it "positions with same row and column are equal" $ do
                Pos 'a' 1 `shouldBe` Pos 'a' 1
            it "positions with different rows are not equal" $ do
                Pos 'a' 1 `shouldNotBe` Pos 'b' 2
            it "positions with different columns are not equal" $ do
                Pos 'a' 1 `shouldNotBe` Pos 'b' 1

        describe "Eq Player" $ do
            it "red players are equal" $ do
                Red `shouldBe` Red
            it "blue players are equal" $ do
                Blue `shouldBe` Blue
            it "red and blue players are not equal" $ do
                Red `shouldNotBe` Blue

        describe "Eq Cell" $ do
            it "empty cells are equal" $ do
                Empty `shouldBe` Empty
            it "stacks with same players are equal" $ do
                Stack [Red] `shouldBe` Stack [Red]
            it "stacks with different players are not equal" $ do
                Stack [Red] `shouldNotBe` Stack [Blue]
            it "empty cells and stacks are not equal" $ do
                Empty `shouldNotBe` Stack [Red] -}