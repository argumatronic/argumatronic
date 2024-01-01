module Main where

import Data.Char

stripUsername :: String -> Maybe String
stripUsername "" = Nothing
stripUsername (x:xs) =
  case (isSpace x || isPunctuation x) of
    True -> stripUsername xs
    -- is recursive to strip off as many leading
    -- whitespaces/punctuations as there are
    False -> Just (x:xs)



mkName :: String -> Maybe Username
mkName name =
  case stripUsername name of
    Nothing -> Nothing
    Just name' ->
      case validateLength 15 name' of
        Nothing -> Nothing
        Just name'' -> Just (Username name'')
    -- wrote this with case statements first,
    -- because we want the input to validateLength to
    -- depend on the output of stripUsername -- that is,
    -- we only want to check the length *after* it's been stripped

mkNm :: String -> Maybe Username
mkNm name = fmap Username (stripUsername name >>= validateLength 15)

-- >>= :: m a -> (a -> m b) -> m b
-- stripUsername name :: Maybe Username ~~ m a (m is Maybe)
-- validateLength :: String -> Maybe Username ~~ (a -> m b)

-- and very similar for the mkPasswd function
mkPasswd :: String -> Maybe Password
mkPasswd pwd = fmap Password (stripSpacePwd pwd >>= validateLength 50)


data User = User Username Password deriving (Eq, Show)

newtype Username =
  Username String deriving (Eq, Show)
newtype Password =
  Password String deriving (Eq, Show)

validUser :: String -> AccValidation [String] Username
validUser n =
    case makeName n of
        Nothing -> AccFailure ["Please enter a valid username."]
        Just name -> AccSuccess name

validPassword :: String -> AccValidation [String] Password
validPassword p =
    case makePassword p of
        Nothing -> AccFailure ["Please enter a valid password."]
        Just password -> AccSuccess password

main :: IO ()
main = do
  name <- getLine
  password <- getLine
  display (makeUser name password)












--mkUser :: String -> String -> AccValidation [String] User
--mkUser n p =
--    User <$> validUser n <*> validPwd p

--display :: AccValidation [String] User -> IO ()
--display avUser =
--  case avUser of
--    AccFailure err -> putStrLn (unlines err)
--    AccSuccess user -> putStrLn "Success!"
