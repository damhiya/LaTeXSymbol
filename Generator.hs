module Generator where

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
  return ()
