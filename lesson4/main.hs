import Data.List

ifEven myFunction x =
  if even x
    then myFunction x
    else x

inc n = n + 1

double n = n * 2

square n = n ^ 2

ifEvenInc n = ifEven inc n

ifEvenDoubl n = ifEven double n

ifEvenSquiare n = ifEven square n

names = [("Ian", "Curtis"), ("Bernard", "Sumner"), ("Peter", "Hook"), ("Stephen", "Morris")]

compareLastNames name1 name2 =
  lastName1 `compare` lastName2
  where
    lastName1 = snd name1
    lastName2 = snd name2

addressLetter name location = locationFunction name
  where
    locationFunction = getLocationFunction location

sfOffice name =
  if lastName < "L"
    then nameText ++ "- PO Box 1234 - San Francisco, CA, 94111"
    else nameText ++ "- PO Box 1010 - San Francisco, CA, 94111"
  where
    lastName = snd name
    nameText = (fst name) ++ " " ++ lastName

nyOffice name = nameText ++ ": PO Box 789"
  where
    nameText = (fst name) ++ " " ++ (snd name)

renoOffice name = nameText ++ "- PO Box 456"
  where
    nameText = (fst name) ++ " " ++ (snd name)

waOffice name = nameText ++ "- PO Box 1234"
  where
    nameText = (fst name) ++ " " ++ (snd name) ++ " Esq"

getLocationFunction location =
  case location of
    "ny" -> nyOffice
    "sf" -> sfOffice
    "reno" -> renoOffice
    "wa" -> waOffice
    _ -> (\name -> (fst name) ++ " " ++ (snd name))
