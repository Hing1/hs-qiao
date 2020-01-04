module GetLine where

myGetLine :: IO String
myGetLine = do
  s <- myGetChar
  return s

myGetChar :: IO String
myGetChar = do
  c <- getChar
  if c == '\n' then return []
  else do
    s <- myGetChar
    return (c:s)