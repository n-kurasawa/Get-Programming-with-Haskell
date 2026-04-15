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
  if lastName1 > lastName2
    then GT
    else
      if lastName1 < lastName2
        then LT
        else EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2

addressLetter name location = nameText ++ " - " ++ location
  where
    nameText = (fst name) ++ " " ++ (snd name)

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
