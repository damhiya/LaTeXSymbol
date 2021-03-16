module Generator where

import Control.Monad
import Data.List
import Data.Map
import System.IO
import Symbols

symbols :: [(Keyword, Font, Symbol)]
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

symbolMap :: Map Keyword [(Font, Symbol)]
symbolMap = unionsWith (++) [singleton k [(f,s)] | (k,f,s) <- symbols]

makeKeyValue :: String -> String -> String
makeKeyValue k v = "\"" ++ k  ++ "\":\"" ++ v ++ "\""

makeObject :: Keyword -> [(Font, Symbol)] -> String
makeObject k xs = "{" ++ intercalate "," elems ++ "}"
  where
    elems = makeKeyValue "keyword" k
          : [makeKeyValue ("symbol-" ++ f) s | (f,s) <- xs]

symbolObjects :: [String]
symbolObjects = foldlWithKey' (\rs k xs -> makeObject k xs : rs) [] symbolMap 

main :: IO ()
main = do
  f <- openFile "symbol_map.json" WriteMode
  hPutStr f "[ "
  hPutStrLn f (intercalate "\n, " symbolObjects)
  hPutStrLn f "]"
  hClose f
