module CidrSuffix (removeCidrSuffix) where

import Data.Char (isDigit)
import Data.List (groupBy)
import Data.Function (on)

isCidrSuffix :: String -> Bool
isCidrSuffix s =
    (all isDigit s && ((length s > 1 && head s /= '0')||(length s == 1)))
    && (t >= 0 && t <= 128)
    where t = read s::Int

removeCidrSuffix :: String -> String
removeCidrSuffix [] = []
removeCidrSuffix s
                  | l == 1 = s
                  | l == 3 =
                      if g !! 1 == "/" && isCidrSuffix (last g)
                      then head g else []
		  | otherwise = []
                  where
                      g = groupBy ((==) `on` (=='/')) s
                      l = length g
