module Bot.Vk
  ( getKeys
  , update
  , updateKeys
  , getAttachment
  , updateAttachments
  , getMsg
  ) where

import Base
import Config
import Config.Get
import Log
import Request.Exception

import qualified Data.Aeson as A

import Control.Monad
--import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTPClient
import qualified Network.HTTP.Client.MultipartFormData as HTTPClient
import qualified Network.HTTP.Simple as HTTPSimple

type Updates = A.Object

type Attachment = A.Object

type TypeName = T.Text

type Message = T.Text

getKeys :: Updates -> App Updates
getKeys = return

update :: ObjApp Message
update = do
  Config.Handle {hLog = logHandle} <- liftApp getApp
  updates <- askSubApp
  liftApp . liftIO . infoM logHandle $ "Updates vk: " <> show updates
  updatePeerId
  updateAttachments
  getMsg

updatePeerId :: ObjApp ()
updatePeerId = do
  updates <- askSubApp
  let userId = getValue ["object", "message", "peer_id"] updates
  liftApp . modifyConfig $ HM.insert "peer_id" userId

updateKeys :: Updates -> App ()
updateKeys updates =
  let key = "ts"
      ts = getValue [key] updates
   in modifyConfig $ HM.insert key ts

getAttachment :: TypeName -> Attachment -> App ()
getAttachment typeName attachmentObj =
  let infoObj = fromObject $ getValue [typeName] attachmentObj
      ownerIdText =
        T.pack . show . valueToInteger $ getValue ["owner_id"] infoObj
      objIdText = T.pack . show . valueToInteger $ getValue ["id"] infoObj
      accessKey =
        case fromString $ getValue ["access_key"] infoObj of
          "" -> ""
          text -> "_" <> text
      attachment = typeName <> ownerIdText <> "_" <> objIdText <> accessKey
   in modifyConfig . insertWithPush "attachment" $ A.String attachment

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
  let typeName = fromString $ getValue ["type"] attachmentObj
  liftIO . debugM logHandle $ "Find vk attachments: " <> T.unpack typeName
  case typeName of
    "sticker" -> handleSticker typeName attachmentObj
    "photo" -> handlePhoto typeName attachmentObj
    "doc" -> handleDoc typeName attachmentObj
    _ -> getAttachment typeName attachmentObj
  handleAttachments rest

handleSticker, handlePhoto, handleDoc :: TypeName -> Attachment -> App ()
handleSticker typeName attachmentObj =
  let field = "sticker_id"
      value = getValue [typeName, field] attachmentObj
   in modifyConfig
        (HM.insert "attachment" (A.String typeName) . HM.insert field value)

handlePhoto typeName attachmentObj = do
  let sizesGot = getValue [typeName, "sizes"] attachmentObj
  let loop = do
        configHandle <- getApp
        responseObj <-
          fromObject . getValue ["response"] . fromObject <$>
          (tryHttpJson $ HTTPSimple.httpJSON =<< getPhotosGetReq configHandle)
        let count = valueToInteger $ getValue ["count"] responseObj
        let offset = valueToInteger . getValue ["offset"] $ hConfig configHandle
        let sizesArrExpected =
              map (getValue ["sizes"]) . fromArrObject $
              getValue ["items"] responseObj
        if any (sizesGot ==) sizesArrExpected
          then getAttachment typeName attachmentObj
          else if offset >= count || count <= 50
                 then uploadPhotosServer (fromArrObject sizesGot) >>
                      getAttachment typeName attachmentObj
                 else modifyConfig (insertWithPush "offset" $ A.Number 50) >>
                      loop
  loop

handleDoc = undefined

uploadPhotosServer :: [A.Object] -> App ()
uploadPhotosServer sizesObjArr = do
  let foldlFunc =
        \ini@(height, width, _) x ->
          let heightX = fromNumber $ getValue ["height"] x
              widhtX = fromNumber $ getValue ["width"] x
              urlX = fromString $ getValue ["url"] x
           in if height < heightX && width < widhtX
                then (heightX, widhtX, urlX)
                else ini
  let (_, _, urlT) = foldl foldlFunc (0, 0, "") sizesObjArr
  let url = T.unpack urlT
  --photoPath <- setPath $ "Photo." <> wordsBy (/='.') url
  --liftIO $ writeFile photoPath =<<
  photoBS <-
    liftIO $
    HTTPClient.responseBody <$>
    (HTTPSimple.httpBS =<< HTTPClient.parseRequest url)
  configHandle1 <- getApp
  responseObj <-
    fromObject . getValue ["response"] . fromObject <$>
    (tryHttpJson $
     HTTPSimple.httpJSON =<< getPhotosGetUploadServerReq configHandle1)
  let uploadUrl =
        HTTPClient.parseRequest_ . T.unpack . fromString $
        getValue ["upload_url"] responseObj
  let formData = HTTPClient.partBS "photo" photoBS
  photosObj <-
    fromObject <$>
    (tryHttpJson $
     HTTPSimple.httpJSON =<< HTTPClient.formDataBody [formData] uploadUrl)
  modifyConfig $ HM.union photosObj
  configHandle2 <- getApp
  void . tryHttpJson $ HTTPSimple.httpJSON =<< getPhotosSaveReq configHandle2

getMsg :: ObjApp Message
getMsg = do
  updates <- askSubApp
  let msg = fromString $ getValue ["object", "message", "text"] updates
  return msg
