module Person (Person, mkAdult, mkChild) where

data Person o
  = Adult
      { firstName :: String,
        lastName :: String,
        email :: String,
        age :: Int,
        occupation :: o
      }
  | Child
      { firstName :: String,
        lastName :: String,
        age :: Int
      }

data Ocupation = Layer | Doctor | Engineer

fullName :: Person o -> String
fullName (Adult fn ln _ _ _) = fn ++ " " ++ ln
fullName (Child fn ln _) = fn ++ " " ++ ln

mkAdult :: String -> String -> String -> Int -> Ocupation -> Person Ocupation
mkAdult = Adult

mkChild :: String -> String -> Int -> Person String
mkChild = Child

personAge :: Person String -> Int
personAge person =
  case person of
    Adult _ _ _ a _ -> a
    Child _ _ a -> a

salesMessage :: Person Ocupation -> String
salesMessage p = case occupation p of
  Layer -> "We'll get your the settlement you deserve"
  Doctor -> "We'll get you the care you need"
  Engineer -> "We'll build the app for you"
