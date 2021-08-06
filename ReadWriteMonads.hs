import           Control.Monad.Reader       (Reader, runReader)
import           Control.Monad.Reader.Class (MonadReader (ask))
import           Control.Monad.Writer
import           Control.Monad.Writer.Class (MonadWriter (tell))
import           Data.Maybe                 (fromMaybe)
import           System.Environment         (lookupEnv)

main :: IO ()
main = do
    env <- loadEnv
    -- let str = func1 env
    let str = runReader func1' env
    print str

data Environment
  = Environment
      { param1 :: String
      , param2 :: String
      , param3 :: String
      }

loadEnv :: IO Environment
loadEnv = do
  p1 <- lookupEnv "param1"
  p2 <- lookupEnv "param2"
  p3 <- lookupEnv "param3"
  return $ Environment
    (fromMaybe "param1" p1)
    (fromMaybe "parameter2" p2)
    (fromMaybe "p3" p3)

func1 :: Environment -> String
func1 env = "Result: " ++ show (func2 env)

func2 :: Environment -> Int
func2 env = 2 + floor (func3 env)

func3 :: Environment -> Float
func3 env = fromIntegral (l1 + l2 + l3) * 2.1
    where
        l1 = length (param1 env)
        l2 = length (param2 env) * 2
        l3 = length (param3 env) * 3

func1' :: Reader Environment String
func1' = do
    res <- func2'
    return ("Result: " ++ show res)

func2' :: Reader Environment Int
func2' = do
    env <- ask
    let res3 = func3 env
    return (2 + floor res3)

instance Semigroup Int where
    a <> b = a * b

instance Monoid Int where
    mempty = 1

acc1' :: String -> (String, Int)
acc1' input = if even (length input)
    then runWriter  (acc2' input)
    else runWriter $ do
        str1 <- acc3' (tail input)
        str2 <- acc4' (take 1 input)
        return (str1 ++ str2)

acc2' :: String -> Writer Int String
acc2' input = if length input > 10
    then do
        tell 1
        acc4' (take 9 input)
    else do
        tell 10
        return input

acc3' :: String -> Writer Int String
acc3' input = if length input `mod` 3 == 0
    then do
        tell 3
        acc2' (input ++ "ab")
    else do
        tell 1
        return $ tail input

acc4' :: String -> Writer Int String
acc4' input = if length input < 10
    then do
        tell (length input)
        return (input ++ input)
    else do
        tell 5
        return (take 5 input)
