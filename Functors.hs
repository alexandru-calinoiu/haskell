data Person
  = Person
      { firstName :: String
      , lastName  :: String
      , age       :: Int
      }

personFromTuple :: (String, String, Int) -> Person
personFromTuple (fName, lName, age) = Person fName lName age

convertTuple :: Maybe (String, String, Int) -> Maybe Person
convertTuple Nothing  = Nothing
convertTuple (Just t) = Just (personFromTuple t)

listFromInputString :: String -> [(String, String, Int)]
listFromInputString contents = mapMaybe tupleFromInputString (lines contents)

tupleFromInputString :: String -> Maybe (String, String, Int)
tupleFromInputString input =
  let stringComponents = words input
      age = read (stringComponents !! 2) :: Int
   in if length stringComponents /= 3
        then Nothing
        else Just (head stringComponents, stringComponents !! 1, age)
