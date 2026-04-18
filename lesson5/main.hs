getRequestUrl host apikey resource id =
  host
    ++ "/"
    ++ id
    ++ "?token="
    ++ apikey

exampleUrlBuilder = getRequestUrl "http://example.com"

myExampleUrlBuilder = exampleUrlBuilder "1237abc"

add4 a b c d = a + b + c + d
