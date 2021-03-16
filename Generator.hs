module Generator where

import Control.Monad
import Data.List
import System.IO
import Symbols

symbols = latexUnaryOperator
       <> latexRelation
       <> latexArrow
       <> latexMisc
       <> latexHebrew
       <> latexParenthesis

       <> superscripts
       <> subscripts
       <> parenthesis
       <> greek
       <> mathBold
       <> mathItalic
       <> mathBoldItalic
       <> mathScript
       <> mathBoldScript
       <> mathFraktur
       <> mathDoubleStruck
       <> mathBoldFraktur
       <> mathSansSerif
       <> mathSansSerifBold
       <> mathSansSerifItalic
       <> mathSansSerifBoldItalic
       <> mathMonospace
       <> mathItalicDotless
       <> mathBoldGreek
       <> mathItalicGreek
       <> mathBoldItalicGreek
       <> mathSansSerifBoldGreek
       <> mathSansSerifBoldItalicGreek
       <> mathBoldDigamma
       <> mathBoldDigit
       <> mathDoubleStruckDigit
       <> mathSansSerifDigit
       <> mathSansSerifBoldDigit
       <> mathMonospaceDigit


main :: IO ()
main = do
  let jsonPairs = map (\(x,y) -> "{\"keyword\":\"" ++ x ++ "\",\"symbol\":\"" ++ y ++ "\"}") symbols
  f <- openFile "symbol_map.json" WriteMode
  hPutStr f "[ "
  hPutStrLn f (intercalate "\n, " jsonPairs)
  hPutStrLn f "]"
  hClose f
