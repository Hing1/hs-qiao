type Person = String
type Book = String
type DataBase = [(Person, Book)]

makeLoan :: DataBase -> Person -> Book -> DataBase
makeLoan db person book = (person, book) : db -- db ++ [(person, book)]

returnLoan :: DataBase -> Person -> Book -> DataBase
returnLoan db person book = [(p, b) | (p, b) <- db, (p, b) /= (person, book)]

books :: DataBase -> Person -> [Book]
books db person = [b | (p, b) <- db, p == person]

numBorrowed :: DataBase -> Person -> Int
numBorrowed db person = length (books db person)

borrowers :: DataBase -> Book -> [Person]
borrowers db book = [p | (p, b) <- db, b == book]

