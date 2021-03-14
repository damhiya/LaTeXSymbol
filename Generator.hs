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
  let jsonPairs = map (\(x,y) -> "{\"key\":\"" ++ x ++ "\",\"value\":\"" ++ y ++ "\"}") symbols
  f <- openFile "symbols.json" WriteMode
  hPutStr f "[ "
  hPutStrLn f (intercalate "\n, " jsonPairs)
  hPutStrLn f "]"
  hClose f
