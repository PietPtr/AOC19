import Data.Char
import Data.Bool
import Debug.Trace

type Password = [Int]


ascending :: Password -> Bool
ascending [char] = True
ascending (char:next:password) = char <= next && (ascending (next:password))

hasdouble :: Password -> Bool
hasdouble [char] = False
hasdouble (char:next:password) = char == next || (hasdouble (next:password))

hasIsolatedPair :: Password -> Int -> Bool
hasIsolatedPair password 0 = False
hasIsolatedPair password r = ( (a /= b) && (b == c) && (c /= d) ) || (hasIsolatedPair rotatedpw (r - 1))
    where
        rotatedpw = (b:c:d:rest) ++ [a]
        (a:b:c:d:rest) = password

padded password = (-1) : password

checkIsolatedPair password = hasIsolatedPair (padded password) 8

ispassword :: Password -> Bool
ispassword password =
    length password == 6 &&
    ascending password &&
    hasdouble password &&
    checkIsolatedPair password


inttopw :: Int -> Password
inttopw intpass = map (\x -> (ord x) - 48) (show intpass)

allstrings :: [Password]
allstrings = map inttopw [138307..654504]

allpasswords = filter (ispassword) allstrings

answer = length allpasswords
