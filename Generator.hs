module Generator where

import Control.Monad
import Data.List
import Data.Map
import System.IO
import Symbols

symbols :: [(Keyword, [Style], Symbol)]
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
       <> doubleStruckGreek
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

symbolMap :: Map Keyword [([Style], Symbol)]
symbolMap = unionsWith (++) [singleton k [(fs,s)] | (k,fs,s) <- symbols]

makeString :: String -> String
makeString s = "\"" ++ s ++ "\""

makePair :: String -> String -> String
makePair k v = makeString k ++ ":" ++ v

makeArray :: [String] -> String
makeArray xs = "[" ++ intercalate "," xs ++ "]"

makeObject :: [String] -> String
makeObject xs = "{" ++ intercalate "," xs ++ "}"

makeElement :: Keyword -> [([Style], Symbol)] -> String
makeElement k xs = makeObject [makePair "keyword" (makeString k), makePair "symbols" symbols]
  where
    symbols = makeArray [
                makeObject [  makePair "style" (makeArray (makeString <$> fs))
                           , makePair "symbol" (makeString s)
                           ]
            | (fs, s) <- xs]

table :: String
table = "[ " ++ intercalate "\n, " styles ++ "\n]"
  where
    styles = foldlWithKey' (\rs k xs -> makeElement k xs : rs) [] symbolMap 

main :: IO ()
main = do
  f <- openFile "symbol_map.json" WriteMode
  hPutStrLn f table
  hClose f
