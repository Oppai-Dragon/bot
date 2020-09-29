module Tests.Bot.Vk
  ( botVkTests
  ) where

import Base
import Bot
import Bot.Vk
import Config
import Log

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

import Test.HUnit

botVkTests :: [Test]
botVkTests =
  [ TestLabel "updateTest" updateTest
  , TestLabel "updatePhotoAttachmentTest" updatePhotoAttachmentTest
  , TestLabel "handleStickerTest" handleStickerTest
  , TestLabel "getMsgTest" getMsgTest
  ]

updateTest, updatePhotoAttachmentTest, handleStickerTest, getMsgTest :: Test
updateTest =
  TestCase $
  runApp (runSubApp update testUpdatesObj) testHandle >>=
  assertEqual
    "for (runApp (runSubApp update testUpdatesObj) testHandle)"
    ("privet", testUpdatedHandle)

updatePhotoAttachmentTest =
  TestCase $
  execApp (updateAttachment "photo" photoAttachmentObj) testHandle >>=
  assertEqual
    "for (execApp (updateAttachment \"photo\" photoAttachmentObj) testHandle >>= return . HM.lookup \"attachment\" . hConfig)"
    (Just $ A.String "photo174435367_457241046_192e2656c063e68c0b") .
  HM.lookup "attachment" . hConfig

handleStickerTest =
  TestCase $
  execApp (handleSticker "sticker" stickerAttachmentObj) testHandle >>= \(Config.Handle {hConfig = config}) ->
    assertEqual
      "for (execApp (handleSticker \"sticker\" stickerAttachmentObj) testHandle >>= \\configHandle {hConfig=config} -> [HM.lookup \"attachment\" config,HM.lookup \"sticker_id\" config])"
      [Just $ A.String "sticker", Just $ A.Number 14090.0]
      [HM.lookup "attachment" config, HM.lookup "sticker_id" config]

getMsgTest =
  TestCase $
  evalApp (runSubApp getMsg testUpdatesObj) testHandle >>=
  assertEqual
    "for (evalApp (runSubApp getMsg testUpdatesObj) testHandle)"
    "privet"

testUpdatedHandle :: Config.Handle
testUpdatedHandle =
  Config.Handle
    {hConfig = testUpdatedConfig, hLog = Log.Handle "" Nothing, hBot = Vk}

testUpdatedConfig, testUpdated, testUpdatesObj, photoAttachmentObj, stickerAttachmentObj, audioAttachmentObj ::
     A.Object
testUpdatedConfig = HM.union testUpdated testConfig

testUpdated =
  HM.fromList
    [ ("attachment", A.String "audio174435367_456241022")
    , ("peer_id", A.Number 1.74435367e8)
    ]

testUpdatesObj =
  HM.fromList
    [ ( "object"
      , A.Object
          (HM.fromList
             [ ( "message"
               , A.Object
                   (HM.fromList
                      [ ( "attachments"
                        , (A.Array . V.fromList) [A.Object audioAttachmentObj])
                      , ("text", A.String "privet")
                      , ("peer_id", A.Number 1.74435367e8)
                      , ("conversation_message_id", A.Number 1643.0)
                      , ("random_id", A.Number 0.0)
                      , ("date", A.Number 1.594990603e9)
                      , ("from_id", A.Number 1.74435367e8)
                      , ("is_hidden", A.Bool False)
                      , ("fwd_messages", A.Array V.empty)
                      , ("id", A.Number 1722.0)
                      , ("important", A.Bool False)
                      , ("out", A.Number 0.0)
                      ]))
             ]))
    , ("group_id", A.Number 1.52071194e8)
    , ("type", A.String "message_new")
    , ("event_id", A.String "81823d9a3a91026138883249450d9f281bbe2ad3")
    ]

photoAttachmentObj =
  HM.fromList
    [ ("album_id", A.Number (-15.0))
    , ("text", A.String "")
    , ("date", A.Number 1.586963467e9)
    , ("access_key", A.String "192e2656c063e68c0b")
    , ("has_tags", A.Bool False)
    , ("id", A.Number 4.57241046e8)
    , ("owner_id", A.Number 1.74435367e8)
    , ( "sizes"
      , A.Array $
        V.fromList
          [ A.Object
              (HM.fromList
                 [ ("height", A.Number 115.0)
                 , ( "url"
                   , A.String
                       "https://sun9-61.userapi.com/c855020/v855020447/225d5f/_0S92XQ2m7w.jpg")
                 , ("width", A.Number 130.0)
                 , ("type", A.String "m")
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 115.0)
                 , ( "url"
                   , A.String
                       "https://sun9-15.userapi.com/c855020/v855020447/225d62/H_rvV-1sCEE.jpg")
                 , ("width", A.Number 130.0)
                 , ("type", A.String "o")
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 177.0)
                 , ( "url"
                   , A.String
                       "https://sun9-17.userapi.com/c855020/v855020447/225d63/c-Bbu5ZIxNs.jpg")
                 , ("width", A.Number 200.0)
                 , ("type", A.String "p")
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 283.0)
                 , ( "url"
                   , A.String
                       "https://sun9-75.userapi.com/c855020/v855020447/225d64/YL2gf-BgaQ0.jpg")
                 , ("width", A.Number 320.0)
                 , ("type", A.String "q")
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 450.0)
                 , ( "url"
                   , A.String
                       "https://sun9-57.userapi.com/c855020/v855020447/225d65/FvUoiNDO160.jpg")
                 , ("width", A.Number 510.0)
                 , ("type", A.String "r")
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 66.0)
                 , ( "url"
                   , A.String
                       "https://sun9-11.userapi.com/c855020/v855020447/225d5e/sohcLrEBmPg.jpg")
                 , ("width", A.Number 75.0)
                 , ("type", A.String "s")
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 533.0)
                 , ( "url"
                   , A.String
                       "https://sun9-25.userapi.com/c855020/v855020447/225d60/vSGE_0wYVgY.jpg")
                 , ("width", A.Number 604.0)
                 , ("type", A.String "x")
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 650.0)
                 , ( "url"
                   , A.String
                       "https://sun9-70.userapi.com/c855020/v855020447/225d61/VpnLLNvM9KU.jpg")
                 , ("width", A.Number 736.0)
                 , ("type", A.String "y")
                 ])
          ])
    ]

stickerAttachmentObj =
  HM.fromList
    [ ( "images"
      , A.Array $
        V.fromList
          [ A.Object
              (HM.fromList
                 [ ("height", A.Number 64.0)
                 , ("url", A.String "https://vk.com/sticker/1-14090-64")
                 , ("width", A.Number 64.0)
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 128.0)
                 , ("url", A.String "https://vk.com/sticker/1-14090-128")
                 , ("width", A.Number 128.0)
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 256.0)
                 , ("url", A.String "https://vk.com/sticker/1-14090-256")
                 , ("width", A.Number 256.0)
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 352.0)
                 , ("url", A.String "https://vk.com/sticker/1-14090-352")
                 , ("width", A.Number 352.0)
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 512.0)
                 , ("url", A.String "https://vk.com/sticker/1-14090-512")
                 , ("width", A.Number 512.0)
                 ])
          ])
    , ("product_id", A.Number 411.0)
    , ( "images_with_background"
      , A.Array $
        V.fromList
          [ A.Object
              (HM.fromList
                 [ ("height", A.Number 64.0)
                 , ("url", A.String "https://vk.com/sticker/1-14090-64b")
                 , ("width", A.Number 64.0)
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 128.0)
                 , ("url", A.String "https://vk.com/sticker/1-14090-128b")
                 , ("width", A.Number 128.0)
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 256.0)
                 , ("url", A.String "https://vk.com/sticker/1-14090-256b")
                 , ("width", A.Number 256.0)
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 352.0)
                 , ("url", A.String "https://vk.com/sticker/1-14090-352b")
                 , ("width", A.Number 352.0)
                 ])
          , A.Object
              (HM.fromList
                 [ ("height", A.Number 512.0)
                 , ("url", A.String "https://vk.com/sticker/1-14090-512b")
                 , ("width", A.Number 512.0)
                 ])
          ])
    , ("sticker_id", A.Number 14090.0)
    ]

audioAttachmentObj =
  HM.fromList
    [ ("type", A.String "audio")
    , ( "audio"
      , A.Object
          (HM.fromList
             [ ( "url"
               , A.String
                   "https://cs9-21v4.vkuseraudio.net/p5/899903aa90615c.mp3?extra=wY1ebEmN2-sMSC_w4FX3IEIQVUdj7fG0hDR83Ae8wv1C45z8RB5rIV2PEuYRNf_Ek0E6iIg6pCjmfuek27On69bZJLeJa6u_2jEFtM9V4ne-v8bQcK9pISBWkgnM9Kh4UyLDAq21H59n9yKqiiUGRdY&long_chunk=1")
             , ( "main_artists"
               , (A.Array . V.fromList)
                   [ A.Object
                       (HM.fromList
                          [ ("domain", A.String "8333364158186615256")
                          , ("name", A.String "Stroke 9")
                          , ("id", A.String "8333364158186615256")
                          ])
                   ])
             , ("date", A.Number 1.592947395e9)
             , ("stories_allowed", A.Bool False)
             , ("id", A.Number 4.56241022e8)
             , ("track_code", A.String "96954ffdNHF4tSqDZAh3OYQXuYDuWqR0fR4")
             , ("short_videos_allowed", A.Bool False)
             , ("title", A.String "Kick Some Ass")
             , ("is_explicit", A.Bool False)
             , ("owner_id", A.Number 1.74435367e8)
             , ("duration", A.Number 240.0)
             , ("artist", A.String "Stroke 9")
             , ("is_focus_track", A.Bool False)
             ]))
    ]
