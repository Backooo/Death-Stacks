-- #############################################################################
-- ####### YOUR UNIT TESTS                                           ###########
-- ####### Note: execute tests using "stack test deathstacks:units"  ###########
-- #############################################################################
import Test.Hspec
import Board
    ( buildBoard,
      validateFEN,
      Cell(Empty,Stack),
      Player(Red, Blue))
import Control.Exception


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

        it "Test 10: too few rows" $ do
            let fen = "r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r"
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"
        it "Test 11: too many rows" $ do
            let fen = "r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r"
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"

        it "Test 12: too few cells" $ do
            let fen = "r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r"
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"

        it "Test 13: too many cells" $ do
            let fen = "r,r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r"
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"

        it "Test 14: only red cells" $ do
            let fen = "r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r"
            let expectedBoard = replicate 6 (replicate 6 (Stack [Red]))
            buildBoard fen `shouldBe` expectedBoard

        it "Test 15: only blue cells" $ do
            let fen = "b,b,b,b,b,b/b,b,b,b,b,b/b,b,b,b,b,b/b,b,b,b,b,b/b,b,b,b,b,b/b,b,b,b,b,b"
            let expectedBoard = replicate 6 (replicate 6 (Stack [Blue]))
            buildBoard fen `shouldBe` expectedBoard

        it "Test 16: too many commas" $ do
            let fen = "r,,,,,,/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r"
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"

        it "Test 17: too few commas" $ do
            let fen = "r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r"
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"

        it "Test 18: very long repeating string" $ do
            let fen = "r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r/r,r,r,r,r,r" ++ concat (replicate 100 "/r,r,r,r,r,r")
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"

        it "Test 19: string with very unusual wrong pattern" $ do
            let fen = "r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b/r,b,r,b,r,b"
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"

        it "Test 20: String with stacks of sizes greater than 1" $ do
            let fen = "rr,rrr,rrrr,rrrrr,rrrrrr,rrrrrrr/rb,rbb,rbbb,rbbbb,rbbbbb,rbbbbbb/rb,rbr,rbrb,rbrbr,rbrbrb,rbrbrbr/br,brb,brbr,brbrb,brbrbr,brbrbrb/br,brr,brrr,brrrr,brrrrr,brrrrrr/bb,bbb,bbbb,bbbbb,bbbbbb,bbbbbbb"
            let expectedBoard = [[Stack (replicate 2 Red), Stack (replicate 3 Red), Stack (replicate 4 Red), Stack (replicate 5 Red), Stack (replicate 6 Red), Stack (replicate 7 Red)],
                     [Stack [Red, Blue], Stack [Red, Blue, Blue], Stack [Red, Blue, Blue, Blue], Stack [Red, Blue, Blue, Blue, Blue], Stack [Red, Blue, Blue, Blue, Blue, Blue], Stack [Red, Blue, Blue, Blue, Blue, Blue, Blue]],
                     [Stack [Red, Blue], Stack [Red, Blue, Red], Stack [Red, Blue, Red, Blue], Stack [Red, Blue, Red, Blue, Red], Stack [Red, Blue, Red, Blue, Red, Blue], Stack [Red, Blue, Red, Blue, Red, Blue, Red]],
                     [Stack [Blue, Red], Stack [Blue, Red, Blue], Stack [Blue, Red, Blue, Red], Stack [Blue, Red, Blue, Red, Blue], Stack [Blue, Red, Blue, Red, Blue, Red], Stack [Blue, Red, Blue, Red, Blue, Red, Blue]],
                     [Stack [Blue, Red], Stack [Blue, Red, Red], Stack [Blue, Red, Red, Red], Stack [Blue, Red, Red, Red, Red], Stack [Blue, Red, Red, Red, Red, Red], Stack [Blue, Red, Red, Red, Red, Red, Red]],
                     [Stack (replicate 2 Blue), Stack (replicate 3 Blue), Stack (replicate 4 Blue), Stack (replicate 5 Blue), Stack (replicate 6 Blue), Stack (replicate 7 Blue)]]
            buildBoard fen `shouldBe` expectedBoard

        it "Test 21: String with no Slashes" $ do
            let fen = ",,,,,,,,,,,,,,,,,,,"
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"

        it "Test 22: String with no Commas" $ do
            let fen = "     /      /      /      /      /     "
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"
        
        it "Test 23: String with wrong colors" $ do
            let fen = "a,b,c,d,e,f/g,h,i,j,k,l/m,n,o,p,q,r/s,t,u,v,w,x/y,z,1,2,3,4/5,6,7,8,9,0"
            evaluate (buildBoard fen) `shouldThrow` errorCall "Invalid FEN string"