{-# LANGUAGE GADTs, KindSignatures #-}

--import Graphics.UI.GLUT

-- Check for valid characters to 16 character Sudoku
minibox :: Char -> Bool
minibox '1' = True
minibox '2' = True
minibox '3' = True
minibox '4' = True
minibox '5' = True
minibox '6' = True
minibox '7' = True
minibox '8' = True
minibox '9' = True
minibox '0' = True
minibox ' ' = True
minibox 'a' = True
minibox 'b' = True
minibox 'c' = True
minibox 'd' = True
minibox 'e' = True
minibox 'f' = True
minibox  _  = False



