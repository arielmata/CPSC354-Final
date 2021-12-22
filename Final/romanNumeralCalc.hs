-- Program that implements a calculator to do basic arithmetic operations over
-- Roman numerals.
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T -- strict type
import qualified Data.Text.IO as TIO -- print T.Text
import qualified Data.Maybe as M -- maybe monad

--------------------------------------------
-- Helper functions on the "Roman Numerals"
--------------------------------------------

-- function that expands a Roman Numeral
expand :: T.Text -> T.Text
expand x
    | T.isPrefixOf (T.pack "IV") x = T.append (T.pack "IIII") (expand (T.drop 2 x))
    | T.isPrefixOf (T.pack "IX") x = T.append (T.pack "VIIII") (expand (T.drop 2 x))
    | T.isPrefixOf (T.pack "XL") x = T.append (T.pack "XXXX") (expand (T.drop 2 x))
    | T.isPrefixOf (T.pack "XC") x = T.append (T.pack "LXXXX") (expand (T.drop 2 x))
    | T.isPrefixOf (T.pack "CD") x = T.append (T.pack "CCCC") (expand (T.drop 2 x))
    | T.isPrefixOf (T.pack "CM") x = T.append (T.pack "DCCCC") (expand (T.drop 2 x))
    | not (T.null x) = T.cons (T.head x) (expand (T.tail x))
    | otherwise = T.empty

-- function that reduces a Roman Numeral
reduce :: T.Text -> T.Text
reduce x
    | T.isSuffixOf (T.pack "VV") x = reduce (romanOrder (T.append (T.dropEnd 2 x) (T.pack "X")))
    | T.isSuffixOf (T.pack "IIIII") x = reduce (romanOrder (T.append (T.dropEnd 5 x) (T.pack "V")))
    | T.isSuffixOf (T.pack "VIIII") x = reduce (T.append (T.dropEnd 5 x) (T.pack "IX"))
    | T.isSuffixOf (T.pack "IIII") x = reduce (T.append (T.dropEnd 4 x) (T.pack "IV"))
    | T.isSuffixOf (T.pack "VIX") x = reduce (T.append (T.dropEnd 3 x) (T.pack "XIV"))
    | T.isSuffixOf (T.pack "LL") x = reduce (romanOrder (T.append (T.dropEnd 2 x) (T.pack "C")))
    | T.isSuffixOf (T.pack "XXXXX") x = reduce (romanOrder (T.append (T.dropEnd 5 x) (T.pack "L")))
    | T.isSuffixOf (T.pack "LXXXX") x = reduce (T.append (T.dropEnd 5 x) (T.pack "XC"))
    | T.isSuffixOf (T.pack "XXXX") x = reduce (T.append (T.dropEnd 4 x) (T.pack "XL"))
    | T.isSuffixOf (T.pack "LXC") x = reduce (T.append (T.dropEnd 3 x) (T.pack "CLX"))
    | T.isSuffixOf (T.pack "DD") x = reduce (romanOrder (T.append (T.dropEnd 2 x) (T.pack "M")))
    | T.isSuffixOf (T.pack "CCCCC") x = reduce (romanOrder (T.append (T.dropEnd 5 x) (T.pack "D")))
    | T.isSuffixOf (T.pack "DCCCC") x = reduce (T.append (T.dropEnd 5 x) (T.pack "CM"))
    | T.isSuffixOf (T.pack "CCCC") x = reduce (T.append (T.dropEnd 4 x) (T.pack "CD"))
    | T.isSuffixOf (T.pack "DCM") x = reduce (T.append (T.dropEnd 3 x) (T.pack "MCD"))
    | not (T.null x) = T.snoc (reduce (T.init x)) (T.last x)
    | otherwise = T.empty

-- function that calls onto itself numerous times to implement the order rules of Roman Numerals
romanOrder :: T.Text -> T.Text
romanOrder x
    | let i = M.fromJust (T.findIndex ('I' ==) x),
    let (front, back) = T.splitAt i x,
    T.isInfixOf (T.singleton 'I') x = T.snoc (romanOrder (T.append front (T.tail back))) 'I'
    | let i = M.fromJust (T.findIndex ('V' ==) x),
    let (front, back) = T.splitAt i x,
    T.isInfixOf (T.singleton 'V') x = T.snoc (romanOrder (T.append front (T.tail back))) 'V'
    | let i = M.fromJust (T.findIndex ('X' ==) x),
    let (front, back) = T.splitAt i x,
    T.isInfixOf (T.singleton 'X') x = T.snoc (romanOrder (T.append front (T.tail back))) 'X'
    | let i = M.fromJust (T.findIndex ('L' ==) x),
    let (front, back) = T.splitAt i x,
    T.isInfixOf (T.singleton 'L') x = T.snoc (romanOrder (T.append front (T.tail back))) 'L'
    | let i = M.fromJust (T.findIndex ('C' ==) x),
    let (front, back) = T.splitAt i x,
    T.isInfixOf (T.singleton 'C') x = T.snoc (romanOrder (T.append front (T.tail back))) 'C'
    | let i = M.fromJust (T.findIndex ('D' ==) x),
    let (front, back) = T.splitAt i x,
    T.isInfixOf (T.singleton 'D') x = T.snoc (romanOrder (T.append front (T.tail back))) 'D'
    | let i = M.fromJust (T.findIndex ('M' ==) x),
    let (front, back) = T.splitAt i x,
    T.isInfixOf (T.singleton 'M') x = T.snoc (romanOrder (T.append front (T.tail back))) 'M'
    | otherwise = T.empty


------------------------------------
-- Arithmetic on the Roman Numerals
------------------------------------

-- function that adds Roman Numerals
addRN :: T.Text -> T.Text -> T.Text
addRN "" n = n
addRN m "" = m
addRN m n = reduce (romanOrder (T.append (expand m) (expand n)))

-- function that decrements a Roman Numeral by one
minusOneRN :: T.Text -> T.Text
minusOneRN x
    | T.isSuffixOf (T.pack "I") x = T.dropEnd 1 x
    | T.isSuffixOf (T.pack "IV") x = T.append (T.dropEnd 2 x) (T.pack "III")
    | T.isSuffixOf (T.pack "V") x = T.append (T.dropEnd 1 x) (T.pack "IV")
    | T.isSuffixOf (T.pack "IX") x = T.append (T.dropEnd 2 x) (T.pack "VIII")
    | T.isSuffixOf (T.pack "X") x = T.append (T.dropEnd 1 x) (T.pack "IX")
    | T.isSuffixOf (T.pack "XL") x = T.append (T.dropEnd 2 x) (T.pack "XXXIX")
    | T.isSuffixOf (T.pack "L") x = T.append (T.dropEnd 1 x) (T.pack "XLIX")
    | T.isSuffixOf (T.pack "XC") x = T.append (T.dropEnd 2 x) (T.pack "LXXXIX")
    | T.isSuffixOf (T.pack "C") x = T.append (T.dropEnd 1 x) (T.pack "XCIX")
    | T.isSuffixOf (T.pack "CD") x = T.append (T.dropEnd 2 x) (T.pack "CCCXCIX")
    | T.isSuffixOf (T.pack "D") x = T.append (T.dropEnd 1 x) (T.pack "CDXCIX")
    | T.isSuffixOf (T.pack "CM") x = T.append (T.dropEnd 2 x) (T.pack "DCCCXCIX")
    | T.isSuffixOf (T.pack "M") x = T.append (T.dropEnd 1 x) (T.pack "CMXCIX")
    | otherwise = T.empty

-- function that subtracts Roman Numerals
subtrRN :: T.Text -> T.Text -> T.Text
subtrRN "" n = T.empty
subtrRN m "" = m
subtrRN m n = subtrRN (minusOneRN m) (minusOneRN n)

-- function that multiplies Roman Numerals
multRN :: T.Text -> T.Text -> T.Text
multRN "" n = T.empty
multRN m "" = T.empty
multRN "I" n = n
multRN m "I" = m
multRN m n = addRN (multRN (minusOneRN m) n) n


----------
-- Testing
----------
main = do
  TIO.putStrLn (addRN "IV" "LXVI") -- LXX
  TIO.putStrLn (addRN "X" "C") -- CX
  TIO.putStrLn (subtrRN "C" "X") -- XC
  TIO.putStrLn (subtrRN "CXXIII" "C") -- XXIII
  TIO.putStrLn (multRN "V" "X") -- L
  TIO.putStrLn (multRN "C" "VI") -- DC
  TIO.putStrLn (multRN "VIII" "CXXV") -- M
