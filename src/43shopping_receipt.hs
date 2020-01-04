module HaskellStore where
import Text.Printf

type Items = [Item]
type Item = (Name, Amount, Price)-- name, amount and price per unit of the item
type Name = String               -- name of the item
type Amount = Float              -- amount, like kg or number
type Price = Float               -- price per unit

printItems :: Items -> IO ()
printItems [] = printf "\nDear, your shopping cart is empty!\n\n"
printItems x = do
    printTitle
    printContent x
    printTotal x

printContent :: Items -> IO()
printContent [] = printf ""
printContent ((n, a, p):xs) = do
    printf "%10s " n
    printf "%10.2f " a
    printf "%10.2f " p
    printf "%10.2f\n" (itemSum (n, a, p))
    printContent xs

itemSum :: Item -> Float
itemSum (n, a, p) = a * p

printTitle :: IO ()
printTitle = do
    printf "\n%10s " "Name"
    printf "%10s " "Amount"
    printf "%10s " "Price"
    printf "%10s\n" "Sum"

printTotal :: Items -> IO ()
printTotal x = do
    printf "-----Total-----------------------"
    printf "%10.2f\n\n" (totalPrice x)

totalPrice :: Items -> Float
totalPrice [] = 0
totalPrice ((n, a, p):xs) = a * p + totalPrice xs

customer1 :: Items
customer1 = [
    ("Apple", 2.50, 5.99),
    ("Bread", 2.00, 3.50)]

customer2 :: Items
customer2 = [
    ("aaaaaaaaaaaaaaaaaaaa", 0, 5.99),
    ("bbbbb", 2.00, 0),
    ("cccccccccc", 2.00, 50)]

customer3 :: Items
customer3 = [
    ("aaaaa", -20.0, 5.99),
    ("bbbbb", 2.00, -50.0),
    ("ccccc", 2.00, 50)]

customer4 :: Items
customer4 = [
    ("aaaaa", 9999999, 5.99),
    ("bbbbb", 2.00, 99999999),
    ("ccccc", 2.00, 50)]

--测试一
--customer1
-- 测试老师提供的用例，确保程序基本正确
--测试二
--customer2
-- 测试字符较长时，程序的运行情况
--测试三
--customer3
-- 测试数字为负时，程序的运行情况
--测试四
--customer4
-- 测试数字较大时，程序的运行情况

--问题一
-- 名字太长时，格式对不齐，如customer2
--问题二
-- 价格和数量没有限制为非负数，如customer3
--问题三
-- 当数量或价格数字太大时会溢出，如customer4