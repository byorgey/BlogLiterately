import           Network.XmlRpc.Client      (remote)
import           Network.XmlRpc.Internals   (Value(..), toValue)
import           Data.Char                  (toLower)
import           System.FilePath            (takeFileName, takeExtension)
import qualified Data.ByteString.Char8 as B
import           Data.Functor               ((<$>))

uploadMediaObject :: FilePath -> IO Value
uploadMediaObject file = do
  media <- mkMediaObject file
  remote "http://mathlesstraveled.com/xmlrpc.php" "metaWeblog.newMediaObject" "default" "byorgey" "a0303017" media

  -- note: same successes + failures with wp.uploadFile in place of
  -- metaWeblog.newMediaObject

-- Create the required struct representing the image.
mkMediaObject :: FilePath -> IO Value
mkMediaObject filePath = do
  bits <- B.unpack <$> B.readFile filePath
  return $ ValueStruct
    [ ("name", toValue fileName)
    , ("type", toValue fileType)
    , ("bits", ValueBase64 bits)
    ]
  where
    fileName = takeFileName filePath
    fileType = case (map toLower . drop 1 . takeExtension) fileName of
                 "png"  -> "image/png"
                 "jpg"  -> "image/jpeg"
                 "jpeg" -> "image/jpeg"
                 "gif"  -> "image/gif"

main = do
  v <- uploadMediaObject "images/checkmark_20.png"
  print v
