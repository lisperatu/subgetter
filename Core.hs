module Subgetter.Core (subsForFile, getSubs, getSubsUrl, openDoc, Subtitle(..)) where
import Text.Printf (printf)
import Data.Text (replace, pack, unpack, Text(..))
import Text.Regex (subRegex, mkRegex, matchRegex)
import System.FilePath (takeBaseName)
import System.Environment (getArgs)
import Text.XML.HXT.Parser.XmlParsec (xread)
import Network.HTTP (getResponseBody, getRequest, simpleHTTP)
import Text.XML.HXT.XPath.XPathEval (getXPath)
import Text.XML.HXT.DOM.TypeDefs
import Text.XML.HXT.DOM.XmlNode (getText)
import Data.Tree.Class (getNode)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BL
import Codec.Archive.Zip 
import Network.HTTP.Base
import Network.URI (parseURI)
import Control.Monad
import Subgetter.Hash (shortsum, showSum)
import System.IO (openBinaryFile, IOMode(..), hFileSize)

-- -- atm: just print xml search results
-- main :: IO ()
-- main = putStrLn . pretty . subs =<< 
--        openDoc . mkUrl =<< 
--        getArgs
--   where
--     pretty = unlines . map (showLine . fromXML)
--     subs = getSubsXml . loadTree


-- Search URL generator
searchUrl :: String -> String -> String
searchUrl langs name = printf urlFormat langs argname
            where
              argname = toArg name
              toArg str = subRegex space str plus
              space = mkRegex " "
              plus = "+"
              urlFormat = "http://www.opensubtitles.org/en/search22/sublanguageid-%s/subformat-srt/moviename-%s/simplexml"
              
-- todo: this are hardcoded for ex-yu languages only, make it universal from some settings
mkUrl :: [String] -> String
mkUrl = searchUrl "bos,css,hrv" . takeBaseName . unwords

-- Load search xml results
openDoc :: String -> IO String
openDoc url = getResponseBody =<< simpleHTTP (getRequest url)



loadTree :: String -> XmlTrees
loadTree = xread . unlines . tail . lines -- read xml without first <?xml ...?> line

getSubsXml :: XmlTrees -> XmlTrees
getSubsXml tree = getXPath "/search/results/subtitle" (head tree)

-- subtitle data and parsing, needed for gui app later
data Subtitle = Subtitle {
  movieName :: String
  , subtitleUrl :: String
  , subtitleId :: String
  , subtitleLang :: String
  , releaseName :: String
  } deriving (Show)
             
-- nice printout
showLine :: Subtitle -> String
showLine sub = printf "%s %s at %s" name lang url where 
  name = movieName s
  lang = subtitleLang s
  url = subtitleUrl s
  s = sub
               
-- convert from xml
fromXML :: XmlTree -> Subtitle
fromXML tree = Subtitle {
  movieName = fromTree "movie" tree
  , subtitleUrl = fromTree "download" tree
  , subtitleLang = fromTree "language" tree
  , subtitleId = fromTree "idsubtitle" tree
  , releaseName = fromTree "releasename" tree }
  where
    fromTree tag tree = fromMaybe "" . getText . getNode . head $ getResult tag tree
    getResult tag tree = getXPath (printf "/subtitle/%s/text()" tag) tree


-- working with subtitle archives
getLazyRequest :: String -> Request BL.ByteString
getLazyRequest url = case parseURI url of
  Nothing -> error "wrong url"
  Just u -> mkRequest GET u
    
getDataStream :: String -> IO BL.ByteString
getDataStream url = getResponseBody =<< simpleHTTP (getLazyRequest url)

saveSubtitle url = do
   content <- getDataStream url
   let archive = toArchive content
   let files = srtFiles $ filesInArchive archive
   let file = files !! 0
   putStrLn $ unlines files
   where
     isSrt file = case matchRegex (mkRegex ".*srt$") file of
       Nothing -> False
       Just _ -> True
     srtFiles = filter isSrt
     
-- working with filesize and hash
otherSearchUrl langs fileSize sum = printf "http://www.opensubtitles.org/en/search22/sublanguageid-%s/subformat-srt/moviebytesize-%d/moviehash-%s/simplexml" langs fileSize sum
 
subsForFile fpath = do
  size <- hFileSize =<< openBinaryFile fpath ReadMode
  hash <- shortsum fpath
  let url = otherSearchUrl "bos,hrv,scc" size (showSum hash)
  (putStrLn . pretty . subs) =<< openDoc url
  where
    pretty = unlines . map (showLine . fromXML)
    subs = getSubsXml . loadTree
    
getSubs = (map fromXML) . getSubsXml . loadTree

getSubsUrl :: String -> IO String
getSubsUrl fpath = do
  size <- hFileSize =<< openBinaryFile fpath ReadMode
  hash <- shortsum fpath
  let url = otherSearchUrl "bos,hrv,scc" size (showSum hash)
  return url
  
main = subsForFile . unwords =<< getArgs
