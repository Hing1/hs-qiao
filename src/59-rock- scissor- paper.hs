module Game where

import           Data.Char
import           System.Random

-- 定义简单石头布类型
data Hand = Rock | Scissor | Paper deriving (Enum)
instance Random Hand where
  random g = case randomR (0,2) g of
                     (r, g') -> (toEnum r, g')
  randomR (a,b) g = case randomR (fromEnum a, fromEnum b) g of
            (r, g') -> (toEnum r, g')
instance Show Hand where
  show Rock    = "石头"
  show Scissor = "剪刀"
  show Paper   = "布"

type Score = (Int, Int)

-- 双方猜拳结果
result :: (Hand, Hand) -> (Int, Int)
result (Rock, Paper)      = (0, 1)
result (Paper, Scissor)   = (0, 1)
result (Scissor, Rock)    = (0, 1)
result (Rock, Rock)       = (0, 0)
result (Scissor, Scissor) = (0, 0)
result (Paper, Paper)     = (0, 0)
result _                  = (1, 0)

-- 游戏一轮流程
playOneRound :: Score -> IO Score
playOneRound s = do
  putStr "请您出手 (R)石头, (S)剪刀, (P)布："
  c <- getChar
  putChar '\n'
  let
    chandio = randomIO :: IO Hand
    in do
      chand <- chandio
      case toLower c of
        'r' -> showRound s (Rock, chand)
        's' -> showRound s (Scissor, chand)
        'p' -> showRound s (Paper, chand)
        _ -> do
          putStrLn "您输入了错误的字母！请重试一遍"
          playOneRound s

-- 显示一轮后的结果
showRound :: Score -> (Hand, Hand) -> IO Score
showRound (p, c) hs = do
  putStrLn $ "我出的是" ++ show (snd hs) ++ "！"
  let
    r@(r1, r2) = result hs
    news = (p + r1, c + r2)
    in
    case r of
      (1, _) -> showScore "您赢了这手！" news
      (_, 1) -> showScore "我赢了这手！" news
      _      -> showScore "这一手平局！" news

-- 显示分数信息
showScore :: String -> Score -> IO Score
showScore str s@(p, c) = do
  putStrLn str
  putStrLn $ "我的得分：" ++ show c
  putStrLn $ "您的得分：" ++ show p
  return s

-- 游戏整个流程
playGame :: Score -> IO ()
playGame s@(p,c) = case (p, c) of
  (3, _) -> putStrLn "72 72 72 72"
  (_, 3) -> putStrLn "73 73 73 73"
  _ -> do
    news <- playOneRound s
    playGame news

play :: IO ()
play = playGame (0, 0)