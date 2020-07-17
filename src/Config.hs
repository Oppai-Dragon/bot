module Config
    ( Config
    , parsePath
    , set
    , setPath
    , setConfig
    , iterateList
    , testConfig
    ) where

import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy   as BSL
import qualified Data.HashMap.Strict    as HM
import qualified Data.Vector            as V
import qualified Data.List              as L
import qualified Data.Text              as T

import           System.Directory (getCurrentDirectory)

type Config = Object

parsePath :: IO (FilePath -> FilePath)
parsePath = return
    ( L.intercalate "\\"
    . takeWhile (/="src")
    . L.words
    . L.intercalate ""
    . map (\x -> if x == "\\" then " " else x)
    . L.group
    )

set :: IO FilePath -> IO Object
set ioPath =
    ioPath
    >>= BSL.readFile
    >>= pure . decode
    >>= \x ->
        case x of
            Just hm -> pure hm
            Nothing -> pure HM.empty

setPath :: FilePath -> IO FilePath
setPath path =
    fmap (flip (<>) $ "\\src\\" <> path)
    $ parsePath <*> getCurrentDirectory

setLogConfig :: IO Config
setLogConfig = set $ setPath "Log\\Log.json"

setConfig :: IO Config
setConfig = do
    config <- set $ setPath "Config.json"
    let bot = case parseMaybe (.: "bot") config of
            Just (String name) -> name
            _                  -> ""
    let botPath = T.unpack $ "Bot\\" <> bot <> "\\" <> bot <> ".json"
    botConfig <- set $ setPath botPath
    logConfig <- setLogConfig
    return $ HM.unions [botConfig,config,logConfig]

iterateList :: (a -> b -> b) -> b -> [a] -> b
iterateList func ini [x]    = func x ini
iterateList func ini (x:xs) = func x $ iterateList func ini xs

testConfig :: Object
testConfig = HM.fromList
    [("bot", String "vk")
    ,("start_request" , (Object . HM.fromList)
        [("path", String "https://api.vk.com/method/groups.getLongPollServer")
        ,("params" , (Array . V.fromList)
            [ "group_id"
            , "access_token"
            , "v"
            ]
            )
        ,("got", String "response")
        ]
     )
    ,("random_id", Number 0)
    ,("message", String "")
    ,("msgField", String "message")
    ,("attachment", String "")
    ,("access_token", String "d044ae47cea77daa95aa6e5a49fe7fbe5c5deb1fdf54eaf13b23c8d5f88fb277de05702378262ef53d1b3")
    ,("keyboard", (Object . HM.fromList)
        [("keyboard", String "{\"one_time\":true,\"buttons\":[[{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"1\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"2\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"3\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"4\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"5\"},\"color\":\"primary\"}]]}")
        ]
     )
    ,("peer_id", Number 200000000099)
    ,("group_id", Number 152071194)
    ,("v", String "5.103")
    ,("repeatN", Number 1)
    ,("repeatMsg", String "At the moment, I repeat what you said times. Press the button with the number, with the desired number of repetitions.")
    ,("helpMsg", String "Hey. I am a simple echo-bot - I write back what they wrote to me. If you want to change how many times I reply to one of your messages, then write /repeat ")
    ,("logLevel", String "DEBUG")
    ]