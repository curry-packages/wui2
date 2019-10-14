-----------------------------------------------------------------------------
--- Solving Su Doku puzzles in Curry with a web-based interface
---
--- Note that this example requires the CLP.FD solver provided with PAKCS
--- and the package for encapsulated search, which can be installed by:
---
---     > cypm add clp-pakcs
---     > cypm add searchtree
---
--- @author Michael Hanus
--- @version October 2019
-----------------------------------------------------------------------------

import Global
import List         ( transpose )
import IO

import CLP.FD     -- requires packages `clp-pakcs`
import Control.AllSolutions ( getOneValue ) -- requires package `searchtree`
import HTML.Base
import HTML.Session
import HTML.WUI

-----------------------------------------------------------------------------
-- Solving a Su Doku puzzle represented as a matrix of numbers (possibly free
-- variables):
sudoku :: [[FDExpr]] -> [Int]
sudoku m = solveFD [FirstFail] (concat m) $
 allC allDifferent m  /\             -- all rows contain different digits
 allC allDifferent (transpose m) /\  -- all columns have different digits
 allC allDifferent (squares m)       -- all 3x3 squares are different
 where
  -- translate a matrix into a list of small 3x3 squares
  squares :: [[a]] -> [[a]]
  squares []            = []
  squares (l1:l2:l3:ls) = group3Rows [l1,l2,l3] ++ squares ls
  
  group3Rows l123 = if null (head l123) then [] else
   concatMap (take 3) l123 : group3Rows (map (drop 3) l123)

-- Compute one solution to a SuDoKu puzzle by encapsulated search:
solveSudoku :: [[Int]] -> [[Int]]
solveSudoku s = toMatrix (sudoku (map (map transDigit) s))
 where transDigit i = if i==0 then head (domain 1 9) else fd i
       toMatrix []       = []
       toMatrix xs@(_:_) = take 9 xs : toMatrix (drop 9 xs)

-- show a solved Su Doku matrix
showSudoku :: [[Int]] -> String
showSudoku = unlines . map (concatMap (\i -> [chr (i + ord '0'),' ']))

-----------------------------------------------------------------------------
-- The specification of the SuDoku WUI:
wSudoku :: WuiSpec [[Int]]
wSudoku = wMatrix (wSelect (\i -> if i==0 then " " else show i) [0..9])

--- The WUI form definition for the puzzle:
wuiForm :: HtmlFormDef (WuiStore [[Int]])
wuiForm = wui2FormDef "Sudoku.wuiForm" sudokuStore wSudoku
                      solvePage wuiSimpleRenderer

--- The data stored for executing the WUI form.
sudokuStore :: Global (SessionStore (WuiStore [[Int]]))
sudokuStore = global emptySessionStore (Persistent "sudokuStore")

-- the main form to input SuDoKu puzzles:
initPage :: [[Int]] -> IO HtmlPage
initPage s = do
  cookie <- sessionCookie  -- be sure that there is a cookie for the session
  setWuiStore sudokuStore s -- initialize WUI store
  return (standardPage "SuDoku" [formExp wuiForm] `addPageParam` cookie)


solvePage :: [[Int]] -> IO [HtmlExp]
solvePage m =
  getOneValue (solveSudoku m) >>=
  return . maybe [h1 [htxt "No solution"]]
                 (\sol -> [h4 [htxt "Solution:"], verbatim $ showSudoku sol])

main :: IO HtmlPage
main = initPage example

example :: [[Int]]
example =
  [ [9,0,0,2,0,0,5,0,0]
  , [0,4,0,0,6,0,0,3,0]
  , [0,0,3,0,0,0,0,0,6]
  , [0,0,0,9,0,0,2,0,0]
  , [0,0,0,0,5,0,0,8,0]
  , [0,0,7,0,0,4,0,0,3]
  , [7,0,0,0,0,0,1,0,0]
  , [0,5,0,0,2,0,0,4,0]
  , [0,0,1,0,0,6,0,0,9] ]

-- An empty Sudoku:
emptyPage :: IO HtmlPage
emptyPage = initPage $ map (const (take 9 (repeat 0))) [1..9]

-- Generate cgi program with:
-- cypm exec curry2cgi -o ~/public_html/cgi-bin/sudoku.cgi Sudoku
