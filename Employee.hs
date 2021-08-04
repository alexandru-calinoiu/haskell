class Person a where
  firstName :: a -> String
  lastName :: a -> String
  age :: a -> Int

getFullName :: (Person a) => a -> String
getFullName p = firstName p ++ " " ++ lastName p

data Employee = Employee
  { employeeFirstName :: String,
    employeeLastName :: String,
    employeeAge :: Int,
    company :: String,
    email :: String,
    salary :: Int
  }

instance Person Employee where
  firstName = employeeFirstName
  lastName = employeeLastName
  age = employeeAge
