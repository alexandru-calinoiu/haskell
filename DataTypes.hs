data Employee
  = Executive String Int Int
  | VicePresident String String Int
  | Manager String String
  | Engineer String Int

data Employer = Employer
  { employerName :: String,
    employerAge :: Int
  }

employee1 :: Employee
employee1 = Executive "Jane Doe" 38 300000

employer1 :: Employer
employer1 = Employer "AF" 10

printName :: Employer -> IO ()
printName employer = putStrLn $ employerName employer

type InterestRate = Float

type BankBalance = Float

applyInterest :: BankBalance -> InterestRate -> BankBalance
applyInterest balance interestRate = balance + (balance * interestRate)