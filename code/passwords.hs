module Main where

import Data.Char (isSpace, isPunctuation, isAlpha)

-- the sample case expression
function :: String -> String
function xs =
  case (xs == "Julie") of
    True -> (xs ++ " is 43.")
    False -> "How old are you?"

-- if you want to play with taking user input, you can try
-- running that function with this `main`. `getLine` is an
-- IO action to get a string from user input and pass it
-- in as an argument. you could
-- write another branch that could take an answer to the
-- printed question and respond depending on various
-- answers. you needn't worry yet about what >>= is. it is
-- called `bind` and it binds the result of `getLine` to
-- the next function called; that is, it lets us pass that
-- result to another function, similar to what function
-- composition does. more on this later.

--main :: IO ()
--main = getLine >>= \name -> putStrLn (function name)

-- strip any *leading* whitespace off the input first
stripSpacePwd :: String -> Maybe String
stripSpacePwd "" = Nothing
stripSpacePwd (x:xs) =
  case (isSpace x) of
    True -> stripSpacePwd xs
    -- is recursive to strip off as many leading
    -- whitespaces/punctuations as there are
    False -> Just (x:xs)

-- check to see that all characters are alphabetic
-- this will return Nothing if there are any other
-- characters in the password, including whitespace
checkAlpha :: String -> Maybe String
checkAlpha "" = Nothing
checkAlpha xs =
  case (all isAlpha xs) of
    False -> Nothing
    True -> Just xs


-- check that the input String is 15 characters or less
validateLength :: String -> Maybe String
validateLength s =
  case (length s > 15) of
    True -> Nothing
    False -> Just s

-- we can't do either of these because the result of each function
-- application is a `Maybe String` which cannot be passed directly
-- into the next function as a `String` argument (none of these
-- accepts a `Maybe String` as an input). these both throw type errors.

--makePwd :: String -> Maybe String
--makePwd xs = validateLength . checkAlpha . stripSpacePwd $ xs

--makePasswd :: String -> Maybe String
--makePasswd xs =  validateLength (checkAlpha (stripSpacePwd xs))


makeP :: String -> Maybe String
makeP xs = case stripSpacePwd xs of
  Nothing -> Nothing
  Just xs' ->
    case checkAlpha xs' of
      Nothing -> Nothing
      Just xs'' ->
        case validateLength xs'' of
          Nothing -> Nothing
          Just xs''' -> Just xs'''

-- but instead of nesting all the cases into one very large function,
-- we can get that nested type of behavior while avoiding the
-- problem that we had with function composition and parenthesized
-- application if we use `bind` (>>=)


makePassword :: String -> Maybe String
makePassword xs = stripSpacePwd xs
                  >>= checkAlpha
                  >>= validateLength

-- you can use `do` syntax with any monad; here
-- our monad is still `Maybe` but this is what
-- the above function makePassword looks like with
-- some `do` sugar on top

makePasswordDo :: String -> Maybe String
makePasswordDo xs = do
  xs'  <- stripSpacePwd xs
  xs'' <- checkAlpha xs'
  validateLength xs''

main :: IO ()
-- without do syntax, using >>= (the monad is IO)
main = getLine >>= (\input -> print (makePassword input))

mainWithDo :: IO ()
mainWithDo = do
  password <- getLine
  print (makePassword password)







