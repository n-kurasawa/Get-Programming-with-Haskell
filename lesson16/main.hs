data AuthorName = AuthorName
  { firstName :: String,
    lastName :: String
  }

data Book = Book
  { author :: Creator,
    isbn :: String,
    bookTitle :: String,
    bookTyear :: Int,
    bookPrice :: Double
  }

data VinyIRecord = VinyIRecord
  { artist :: Creator,
    recordTitle :: String,
    recordYear :: Int,
    recordPrice :: Double
  }

data CollectibleToy = CollectibleToy
  { name :: String,
    description :: String,
    toyPrice :: Double
  }

data Pamphlet = Pamphlet
  { panpletTitle :: String,
    panhpletDescription :: String,
    contact :: String
  }

data StoreItem
  = BookItem Book
  | RecordItem VinyIRecord
  | ToyItem CollectibleToy
  | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0

madeBy :: StoreItem -> String
madeBy (BookItem book) = show (author book)
madeBy (RecordItem record) = show (artist record)
madeBy _ = ""

type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char
  deriving (Show)

data Creator = AuthorCreator Author | ArticeCreator Artist deriving (Show)

data Author = Author Name deriving (Show)

data Artist = Person Name | Band String deriving (Show)

hpLovecraft :: Creator
hpLovecraft =
  AuthorCreator
    ( Author
        (TwoInitialsWithLast 'H' 'P' "Lovecraft")
    )

type Radius = Double

type Height = Double

type Width = Double

data Shape = Circle Radius | Square Height | Rectangle Height Width deriving (Show)

perimeter :: Shape -> Double
perimeter (Circle r) = 2 * pi * r
perimeter (Square h) = h * 4
perimeter (Rectangle h w) = h * 2 + w * 2

area :: Shape -> Double
area (Circle r) = pi * r ^ 2
area (Square h) = h ^ 2
area (Rectangle h w) = h * w
