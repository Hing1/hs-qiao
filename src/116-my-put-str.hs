module PutStr where

myPutStr :: String -> IO ()
myPutStr s = do
  myPutChar s

myPutChar :: String -> IO ()
myPutChar [] = putChar '\n'
myPutChar (x:xs) = do
  putChar x
  myPutChar xs