data AuthorName = AuthorName
  { firstName :: String,
    lastName :: String
  }

data Book = Book
  { author :: AuthorName,
    isbn :: String,
    title :: String,
    year :: Int,
    price :: Double
  }

type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  | FirstNameWithTwoInits FirstName Char Char

data Creator = AuthorCreator Author | ArticeCreator Artist

data Author = Author Name

data Artist = Person Name | Band String

hpLovecraft :: Creator
hpLovecraft =
  AuthorCreator
    ( Author
        (TwoInitialsWithLast 'H' 'P' "Lovecraft")
    )
