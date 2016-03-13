import           Network.HTTP      (getRequest, getResponseBody, simpleHTTP)
import           Text.HTML.TagSoup

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

getLucky :: String -> IO String
getLucky searchTerm = do
  results <- openURL $ "http://www.google.com/search?q=" ++ searchTerm
  let tags   = parseTags results
      anchor = take 1 . dropWhile (~/= "<a>") . dropWhile (~/= "<h3 class='r'>") $ tags
  let url = case anchor of
              [t@(TagOpen{})] -> takeWhile (/='&') . dropWhile (/='h') . fromAttrib "href" $ t
              _ -> "XXXXXXXXXX"
  return url

main :: IO ()
main = do
  url <- getLucky "cats"
  print url
