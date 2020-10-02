module Bot.Vk
  ( getKeys
  , update
  , updateKeys
  , updateAttachment
  , updateAttachments
  , handleSticker
  , uploadAttachmentServer
  , setAttachmentUrl
  , getMsg
  ) where

import Base
import Config
import Config.Get
import Log
import Request.Exception

import Control.Monad
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Client.MultipartFormData as HTTPClient
import qualified Network.HTTP.Simple as HTTPSimple
import qualified System.Directory as Dir

type Updates = A.Object

type Attachment = A.Object

type InfoObj = A.Object

type ResponseObj = A.Object

type TypeName = T.Text

type Message = T.Text

type MessageObj = A.Object

type Url = String

getKeys :: Updates -> App Updates
getKeys = return

update :: ObjApp Message
update = do
  Config.Handle {hLog = logHandle} <- liftApp getApp
  updates <- askSubApp
  liftApp . liftIO . infoM logHandle $ "Updates vk: " <> show updates
  liftApp clearAttachments
  updateSendParams
  updateAttachments
  getMsg

updateSendParams :: ObjApp ()
updateSendParams = do
  updates <- askSubApp
  let messageObj = fromObject $ getValue ["object", "message"] updates
  liftApp $ do
    updatePeerId messageObj
    when (isFwdMsg messageObj) $ updateFwdMsg messageObj

updatePeerId :: MessageObj -> App ()
updatePeerId =
  let field = "peer_id"
   in modifyConfig . HM.insert field . getValue [field]

isFwdMsg :: MessageObj -> Bool
isFwdMsg = not . null . fromArrObject . getValue ["fwd_messages"]

updateFwdMsg :: MessageObj -> App ()
updateFwdMsg messageObj = do
  responseobj <- updateConversationMsgIds messageObj
  let fwdMsg =
        A.String .
        T.intercalate "," . map (toText . getValue ["id"]) . fromArrObject $
        getValue ["items"] responseobj
  modifyConfig $ HM.insert "forward_messages" fwdMsg

updateConversationMsgIds :: MessageObj -> App ResponseObj
updateConversationMsgIds messageObj = do
  let conversationMsgIds =
        A.String .
        T.intercalate "," .
        map (toText . getValue ["conversation_message_id"]) . fromArrObject $
        getValue ["fwd_messages"] messageObj
  modifyConfig $ HM.insert "conversation_message_ids" conversationMsgIds
  configHandle <- getApp
  fmap fromObject . tryUnpackResponseHttpJson $
    HTTPSimple.httpJSON =<< getMsgGetByConversationMsgId configHandle

clearAttachments :: App ()
clearAttachments = do
  modifyConfig $ HM.delete "forward_messages"
  modifyConfig $ HM.delete "attachment"

updateKeys :: Updates -> App ()
updateKeys updates =
  let key = "ts"
      ts = getValue [key] updates
   in modifyConfig $ HM.insert key ts

updateAttachment :: TypeName -> Attachment -> App ()
updateAttachment typeName attachmentObj =
  let ownerIdText = toText $ getValue ["owner_id"] attachmentObj
      objIdText = toText $ getValue ["id"] attachmentObj
      accessKey =
        case fromString $ getValue ["access_key"] attachmentObj of
          "" -> ""
          text -> "_" <> text
      attachment = typeName <> ownerIdText <> "_" <> objIdText <> accessKey
   in do configHandle <- getApp
         liftIO . debugM (hLog configHandle) $
           "Vk attachment: " <> T.unpack attachment
         modifyConfig . insertWithPush "attachment" $ A.String attachment

updateAttachments :: ObjApp ()
updateAttachments = do
  updates <- askSubApp
  let attachmentsObj = fromObject $ getValue ["object", "message"] updates
  let attachmentsObjArr =
        fromArrObject $ getValue ["attachments"] attachmentsObj
  liftApp $ handleAttachments attachmentsObjArr
  Config.Handle {hConfig = config, hLog = logHandle} <- liftApp getApp
  let attachments = getValue ["attachment"] config
  liftApp . liftIO . debugM logHandle $
    "Vk attachments: " <> (T.unpack . fromString) attachments

handleAttachments :: [Attachment] -> App ()
handleAttachments [] = return ()
handleAttachments (attachmentObj:rest) = do
  Config.Handle {hLog = logHandle} <- getApp
  let typeNameValue = getValue ["type"] attachmentObj
  let typeName = fromString typeNameValue
  let infoObj = fromObject $ getValue [typeName] attachmentObj
  liftIO . debugM logHandle $ "Find vk attachments: " <> T.unpack typeName
  modifyConfig $ HM.insert "type" typeNameValue
  case typeName of
    "sticker" -> handleSticker infoObj
    "photo" -> uploadAttachmentServer typeName infoObj
    "doc" -> uploadAttachmentServer typeName infoObj
    "audio_message" -> uploadAttachmentServer typeName infoObj
    "audio" -> updateAttachment typeName infoObj
    "video" -> updateAttachment typeName infoObj
    _ ->
      liftIO . warningM logHandle $
      "Unknown type of attachment: " <> T.unpack typeName
  handleAttachments rest

handleSticker :: Attachment -> App ()
handleSticker attachmentObj =
  let typeName = "sticker"
      field = "sticker_id"
      value = getValue [field] attachmentObj
   in do configHandle <- getApp
         liftIO . debugM (hLog configHandle) $
           "Sticker_id: " <> (T.unpack . toText) value
         modifyConfig $ HM.insert field value
         modifyConfig . HM.insert "attachment" $ A.String typeName

uploadAttachmentServer :: TypeName -> InfoObj -> App ()
uploadAttachmentServer typeName infoObj = do
  modifyConfig . HM.insert "ext" $ getValue ["ext"] infoObj
  url <- setAttachmentUrl typeName infoObj
  let title = getValue ["title"] infoObj
  modifyConfig $ HM.insert "title" title
  configHandle <- getApp
  attachmentPath <- liftIO $ createAttachmentFile typeName url configHandle
  responseObj <- attachmentGetMsgUploadServer typeName
  addAttachmentObj typeName attachmentPath responseObj
  liftIO $ Dir.removeFile attachmentPath
  attachmentObj <- attachmentSave typeName
  updateAttachment typeName attachmentObj

createAttachmentFile :: TypeName -> Url -> Config.Handle -> IO FilePath
createAttachmentFile typeName url Config.Handle { hConfig = config
                                                , hLog = logHandle
                                                } = do
  let title = T.unpack . fromString $ getValue ["title"] config
  let parseFunc x =
        case typeName of
          "photo" -> x <> "\\Photo." <> (last . wordsBy (/= '.')) url
          "audio_message" ->
            x <> "\\AudioMessage." <> (last . wordsBy (/= '.')) url
          _ ->
            x <>
            "\\" <>
            (intercalate "." . take 2 . wordsBy (/= '.'))
              (title <> "." <> (T.unpack . fromString . getValue ["ext"]) config)
  attachmentBS <-
    fmap HTTPClient.responseBody $
    HTTPSimple.httpBS =<<
    tryParseRequest (HTTPClient.parseRequest url) logHandle
  attachmentPath <- parseFunc <$> getRepDir
  BS.writeFile attachmentPath attachmentBS
  return attachmentPath

setAttachmentUrl :: TypeName -> InfoObj -> App Url
setAttachmentUrl typeName infoObj = do
  Config.Handle {hLog = logHandle} <- getApp
  url <-
    (=<<) checkUrl $
    case typeName of
      "photo" -> return $ getPhotoUrl infoObj
      "doc" -> return $ getDocUrl infoObj
      "audio_message" -> return $ getAudioMsgUrl infoObj
      _ -> do
        liftIO . errorM logHandle $
          "Unknown type of attachment: " <> T.unpack typeName
        return ""
  liftIO . debugM logHandle $ T.unpack typeName <> " url: " <> url
  return url

checkUrl :: Url -> App Url
checkUrl url =
  case url of
    "" -> do
      configHandle <- getApp
      liftIO $ errorM (hLog configHandle) "Failed on getting url of attachment"
      return ""
    _ -> return url

getPhotoUrl, getDocUrl, getAudioMsgUrl :: InfoObj -> Url
getPhotoUrl infoObj =
  let sizesObjArr = fromArrObject $ getValue ["sizes"] infoObj
      foldlFunc =
        \ini@(height, width, _) x ->
          let heightX = fromNumber $ getValue ["height"] x
              widhtX = fromNumber $ getValue ["width"] x
              urlX = fromString $ getValue ["url"] x
           in if height < heightX && width < widhtX
                then (heightX, widhtX, urlX)
                else ini
      (_, _, urlT) = foldl foldlFunc (0, 0, "") sizesObjArr
   in T.unpack urlT

getDocUrl = T.unpack . fromString . getValue ["url"]

getAudioMsgUrl = T.unpack . fromString . getValue ["link_mp3"]

attachmentGetMsgUploadServer :: TypeName -> App ResponseObj
attachmentGetMsgUploadServer typeName = do
  configHandle@Config.Handle {hLog = logHandle} <- getApp
  resultObj <-
    fmap fromObject . tryUnpackResponseHttpJson . (=<<) HTTPSimple.httpJSON $
    case typeName of
      "doc" -> getDocsGetMsgUploadServerReq configHandle
      "audio_message" -> getDocsGetMsgUploadServerReq configHandle
      "photo" -> getPhotosGetMsgUploadServerReq configHandle
      _ -> do
        errorM logHandle $ "Unknown type of attachment: " <> T.unpack typeName
        return HTTPSimple.defaultRequest
  liftIO . debugM logHandle $ "Upload object: " <> show resultObj
  return resultObj

attachmentSave :: TypeName -> App ResponseObj
attachmentSave typeName = do
  configHandle@Config.Handle {hLog = logHandle} <- getApp
  let handleResponseObj =
        case typeName of
          "audio_message" ->
            return . fromObject . getValue [typeName] . fromObject
          "doc" -> return . fromObject . getValue [typeName] . fromObject
          "photo" -> return . head . fromArrObject
          _ ->
            \_ -> do
              liftIO . errorM logHandle $
                "Unknown type of attachment: " <> T.unpack typeName
              return HM.empty
  responseObj <-
    (=<<) handleResponseObj .
    tryUnpackResponseHttpJson . (=<<) HTTPSimple.httpJSON $
    case typeName of
      "doc" -> getDocsSaveReq configHandle
      "audio_message" -> getDocsSaveReq configHandle
      "photo" -> getPhotosSaveMsgPhotoReq configHandle
      _ -> do
        errorM logHandle $ "Unknown type of attachment: " <> T.unpack typeName
        return HTTPSimple.defaultRequest
  liftIO . debugM logHandle $
    T.unpack typeName <> " save object: " <> show responseObj
  return responseObj

addAttachmentObj :: TypeName -> FilePath -> ResponseObj -> App ()
addAttachmentObj typeName attachmentPath responseObj = do
  Config.Handle {hLog = logHandle} <- getApp
  let url = T.unpack . fromString $ getValue ["upload_url"] responseObj
  request <- liftIO $ tryParseRequest (HTTPClient.parseRequest url) logHandle
  partName <- choosePartName typeName
  let formData = HTTPClient.partFile partName attachmentPath
  attachmentJson <-
    tryHttpJson $
    HTTPSimple.httpJSON =<< HTTPClient.formDataBody [formData] request
  let attachmentObj = fromObject attachmentJson
  liftIO . debugM logHandle $
    T.unpack typeName <> " json: " <> show attachmentJson
  case typeName of
    "doc" -> modifyConfig $ HM.union attachmentObj
    "audio_message" -> modifyConfig $ HM.union attachmentObj
    "photo" -> modifyConfig . HM.union $ HM.singleton "photo_obj" attachmentJson
    _ -> do
      liftIO . errorM logHandle $
        "Unknown type of attachment: " <> T.unpack typeName
      return ()

choosePartName :: TypeName -> App TypeName
choosePartName typeName =
  case typeName of
    "photo" -> return "photo"
    "doc" -> return "file"
    "audio_message" -> return "file"
    _ -> do
      Config.Handle {hLog = logHandle} <- getApp
      liftIO . errorM logHandle $
        "Unknown type of attachment: " <> T.unpack typeName
      return ""

getMsg :: ObjApp Message
getMsg = do
  updates <- askSubApp
  Config.Handle {hConfig = config} <- liftApp getApp
  let msg = fromString $ getValue ["object", "message", "text"] updates
  let typeName = fromString $ getValue ["type"] config
  return $
    case typeName of
      "graffiti" -> "Cannot send graffiti, it's only for users"
      _ -> msg
