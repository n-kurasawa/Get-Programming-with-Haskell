{- cabal:
build-depends: base, containers
-}

import Data.List
import Data.Map qualified as Map

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2, 7, 13, 14, 21, 24]

organPairs :: [(Int, Organ)]
organPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organPairs

possibleDrawers :: [Int]
possibleDrawers = [1 .. 50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
  where
    getContents = \id -> Map.lookup id catalog

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ avaiable =
  length
    ( filter
        (\x -> x == Just organ)
        avaiable
    )

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justThaOragans :: [Maybe Organ]
justThaOragans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justThaOragans

cleanList :: String
cleanList = intercalate "," organList

numOrZero :: Maybe Int -> Int
numOrZero Nothing = 0
numOrZero (Just num) = num
