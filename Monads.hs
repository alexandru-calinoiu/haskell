import qualified Data.Char
maybeFunc1 :: String -> Maybe Int
maybeFunc1 ""  = Nothing
maybeFunc1 str = Just $ length str

maybeFunc2 :: Int -> Maybe Float
maybeFunc2 i =
  if even i
    then Nothing
    else Just (fromIntegral i * 3.14159)

maybeFunc3 :: Float -> Maybe [Int]
maybeFunc3 f =
  if f > 15.0
    then Nothing
    else Just [floor f, ceiling f]

runMaybeFuncBind :: String -> Maybe [Int]
runMaybeFuncBind input = maybeFunc1 input >>= maybeFunc2 >>= maybeFunc3

runMaybeFuncBindDo :: String -> Maybe [Int]
runMaybeFuncBindDo input = do
  i <- maybeFunc1 input
  f <- maybeFunc2 i
  maybeFunc3 f

eitherFunc1 :: String -> Either String Int
eitherFunc1 ""  = Left "String connot be empty"
eitherFunc1 str = Right $ length str

eitherFunc2 :: Int -> Either String Float
eitherFunc2 i = if even i
    then Left "Length cannot be even!"
    else Right (fromIntegral i * 3.14159)

eitherFunc3 :: Float -> Either String [Int]
eitherFunc3 f = if f > 15.0
    then Left "Float is to large"
    else Right [floor f, ceiling  f]

runEitherFuncs :: String -> Either  String [Int]
runEitherFuncs input = do
    i <- eitherFunc1 input
    f <- eitherFunc2 i
    eitherFunc3 f

main :: IO ()
main = do
    input <- getLine
    let uppercased = map Data.Char.toUpper input
    print uppercased
