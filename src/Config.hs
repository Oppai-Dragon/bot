module Config
    ( Config
    , parsePath
    , set
    , setPath
    , setConfig
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

setConfig :: IO Config
setConfig = do
    config <- set $ setPath "MyConfig.json"
    let bot = case parseMaybe (.: "bot") config of
            Just (String name) ->
                case parseMaybe (.: name) config of
                    Just (Object obj) -> obj
                    _                 -> HM.empty
            _                 -> HM.empty
    let withoutBots =
            case parseMaybe (.: "bots") config :: Maybe [T.Text] of
                Just textArr -> iterateList HM.delete config textArr
                _            -> config
    let botConfig = HM.union withoutBots bot
    return botConfig

iterateList :: (a -> b -> b) -> b -> [a] -> b
iterateList func ini [x]    = func x ini
iterateList func ini (x:xs) = func x $ iterateList func ini xs

testConfig = object
    ["bot" .= String "telegram"
    , "vk" .= object
        [ "start_request" .= object
            [ "path" .= String "https://api.vk.com/method/groups.getLongPollServer?"
            , "params" .= (Array . V.fromList . map String)
                [ "group_id"
                , "access_token"
                , "v"
                ]
            ]
        , "send_request" .= object
            [ "path" .= String "https://api.vk.com/method/message.send?"
            , "params" .= (Array . V.fromList . map String)
                [ "user_id"
                , "random_id"
                , "peer_id"
                , "message"
                , "keyboard"
                , "access_token"
                , "v"
                ]
            ]
        , "access_token" .= String "d044ae47cea77daa95aa6e5a49fe7fbe5c5deb1fdf54eaf13b23c8d5f88fb277de05702378262ef53d1b3"
        , "keyboard" .= String "{\"one_time\":true,\"buttons\":[[{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"1\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"2\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"3\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"4\"},\"color\":\"primary\"},{\"action\":{\"type\":\"text\",\"payload\":\"{\\\"button\\\": \\\"1\\\"}\",\"label\":\"5\"},\"color\":\"primary\"}]]}"
        , "peer_id" .= String "200000000099"
        , "group_id" .= String "152071194"
        , "v" .= String "5.103"
        ]
    , "telegram" .= object
        [ "start_uri" .= String "https://api.telegram.org/bot<access_token>/getUpdates?"
        , "timeout" .= String "30"
        , "access_token" .= String "1222090060:AAG110wvYURl-eheQ2eIyDCSWaY3KWxve0s"
        , "keyb" .= String "{\"keyboard\":[[{\"text\":\"1\"},{\"text\":\"2\"},{\"text\":\"3\"},{\"text\":\"4\"},{\"text\":\"5\"}]],\"resize_keyboard\":true,\"one_time_keyboard\":true}"
        , "proxyHost" .= String "59.29.245.151"
        , "proxyPort" .= String "3128"
        , "offset" .= String ""
        , "chat_id" .= String ""
        , "isSendMessage" .= String "True"
        ]
    , "repeatN" .= String "1"
    , "lastMsg" .= String ""
    , "repeatMsg" .= String "At the moment, I repeat what you said times. Press the button with the number, with the desired number of repetitions."
    , "helpMsg" .= String "Hey. I am a simple echo-bot - I write back what they wrote to me. If you want to change how many times I reply to one of your messages, then write /repeat "
    , "testMsg" .= String "Please use one human language: Russian or English. If you write in Russian, please use only Russian letters."
    , "logLevel" .= String "DEBUG"
    ]