module Lib
    (
    gen
    ) where

import Data.List(intercalate)
import System.Environment(getArgs)
import System.Exit
import Text.Read(readMaybe)
import Data.List.Split(chunksOf)

type PSFile = String

--------------------------------------------------------------------------------
--top level

gen :: IO ()
gen = do
  putStrLn "Example Usage:"
  putStrLn "./ps-record-gen-exe \"FILENAME.purs\" \"TYPEALIAS\" \"TYPECON\" \"DATACON\" \"LABEL1\" \"TYPE1\" \"LABEL2\" \"TYPE2\" ..."
  args <- getArgs
  let len = length args
  if len >= 6 && even len
  then do
    let [fileName, typeAlias, typeCon, dataCon] = take 4 args
        list = chunksOf 2 $ drop 4 args
        tlist = package <$> list
    success fileName typeAlias typeCon dataCon tlist
  else failure

package :: [a] -> (a, a)
package [x, y] = (x, y)

failure :: IO ()
failure = do
  putStrLn "Failed To Parse Arguments"
  exitFailure

success :: FilePath -> String -> String -> String -> [(String, String)] -> IO ()
success f ta tc dc l = do
  let text = fileText ta tc dc l
  writeFile f text
  putStrLn "Generation Successful!"
  exitSuccess

fileText :: String -> String -> String -> [(String, String)] -> PSFile
fileText ta tc dc l = concat $ [
    message,
    boilerplate,
    typeline ta l,
    newtypeline ta tc dc,
    showinstance tc dc,
    constructor l tc dc,
    lifter ta tc dc,
    l >>= (uncurry $ fieldText tc)
  ]

variablename :: Int -> String
variablename n = (toEnum $ r + 97) : (if q > 0 then show (q + 1) else "")
  where (q, r) = quotRem n 26

linebreak :: String
linebreak = "--------------------------------------------------------------------------------"

--------------------------------------------------------------------------------
--single strings

message :: String
message = concat [linebreak, "\n-- all generated functions are based on the following template:\n\n-- modify_fieldName' :: forall a b c. (a -> b) -> {fieldName :: a | c} -> {fieldName :: b | c}\n-- modify_fieldName' f rec = rec {fieldName = f rec.fieldName}\n", linebreak, "\n\n"]

boilerplate :: String
boilerplate = "module Data.Record.Gen where\nimport Prelude\n\n"

typeline :: String -> [(String, String)] -> PSFile
typeline ta l = concat ["type ", ta, " = {", (init $ init $ l >>= f), "}\n\n"]
  where f (l, t) = concat [l, " :: ", t, ", "]

newtypeline :: String -> String -> String -> PSFile
newtypeline ta tc dc = concat ["newtype ", tc, " = ", dc, " ", ta, "\n\n"]

showinstance :: String -> String -> PSFile
showinstance tc dc = concat ["instance show", tc, " :: Show ", tc, " where\n\tshow (", dc, " a) = show a\n\n"]

constructor :: [(String, String)] -> String -> String -> PSFile
constructor l tc dc = concat ["new_", tc, " :: ", l >>= (f.snd), tc, "\nnew_", tc, " ",
                      intercalate " " (snd <$> l'), " = ", dc, " {", (init $ init $ l' >>= g), "}\n\n"]
  where f t = t ++ " -> "
        l' = zip (fst <$> l) (variablename <$> [0..(length l - 1)])
        g (l, v) = concat [l, " : ", v, ", "]

lifter :: String -> String -> String -> PSFile
lifter ta tc dc = concat ["liftR :: (", ta, " -> ", ta, ") -> ", tc, " -> ", tc, "\nliftR f (", dc, " a) = ", dc, " $ f a\n\n"]

--------------------------------------------------------------------------------
--per field string

fieldText :: String -> String -> String -> PSFile
fieldText tc l t = concat $ [
    seperator l,
    field l,
    modify l t tc,
    update l t tc
  ]

seperator :: String -> PSFile
seperator l = concat [linebreak, "\n--", l, "\n\n"]

field :: String -> PSFile
field l = concat ["modify_", l, "' :: forall a b c. (a -> b) -> {", l, " :: a | c} -> {", l, " :: b | c}\nmodify_", l, "' f rec = rec {", l, " = f rec.", l, "}\n\n"]

modify :: String -> String -> String -> PSFile
modify l t tc = concat ["modify_", l, " :: (", t, " -> ", t, ") -> ", tc, " -> ", tc, "\nmodify_", l, " = liftR <<< modify_", l, "'\n\n"]

update :: String -> String -> String -> PSFile
update l t tc = concat ["update_", l, " :: ", t, " -> ", tc, " -> ", tc, "\nupdate_", l, " = liftR <<< modify_", l, "' <<< const\n\n"]
