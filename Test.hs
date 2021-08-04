{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Map as M

factorial :: (Eq p, Num p) => p -> p
factorial 0 = 1
factorial n = n * factorial (n - 1)

secsToWeeks :: (Fractional (p1 -> p2), Num p2) => (p1 -> p2) -> p1 -> p2
secsToWeeks secs =
  let perMinute = 60
      perHour = 60 * perMinute
      perDay = 24 * perHour
      perWeek n = 7 * perDay
   in secs / perWeek

classify age = case age of
  0 -> "newborn"
  1 -> "infant"
  2 -> "toddler"
  _ -> "senior"

errorsPerLine =
  M.fromList
    [("Chris", 472), ("Don", 100), ("Simon", -5)]

main = do
  putStrLn "Who are you?"
  name <- getLine
  case M.lookup name errorsPerLine of
    Nothing -> putStrLn "I don't know"
    Just n -> do
      putStr "Erros per line: "
      print n
