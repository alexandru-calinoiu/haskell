class (Monad m) => MyLoggerMonad m where
  prevMessages :: m [String]
  logString :: String -> m ()
