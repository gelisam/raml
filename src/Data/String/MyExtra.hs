module Data.String.MyExtra where

import Data.Char
import Text.Printf


-- |
-- >>> capitalize "fooBar"
-- "FooBar"
capitalize :: String -> String
capitalize [] = error "can't capitalize the empty string"
capitalize (c:cs) = toUpper c : cs

-- |
-- CamelCase concatenation
-- 
-- >>> "foo" +++ "bar"
-- "fooBar"
(+++) :: String -> String -> String
s1 +++ s2 = s1 ++ capitalize s2

-- |
-- dot concatenation
-- 
-- >>> "foo" .++ "bar"
-- "foo.bar"
(.++) :: String -> String -> String
(.++) = printf "%s.%s"
