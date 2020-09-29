module Bot.Vk
  ( getKeys
  , update
  , updateKeys
  , updateAttachment
  , updateAttachments
  , handleSticker
  , uploadPhotosServer
  , getMsg
  ) where

import Base
import Config
import Config.Get
import Log
import Request.Exception

import qualified Data.Aeson as A
import qualified Data.ByteString as BS
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
  liftApp . modifyConfig $ HM.delete "attachment"
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

updateAttachment :: TypeName -> Attachment -> App ()
updateAttachment typeName attachmentObj =
  let ownerIdText = toText $ getValue ["owner_id"] attachmentObj
      objIdText = toText $ getValue ["id"] attachmentObj
      accessKey =
        case fromString $ getValue ["access_key"] attachmentObj of
          "" -> ""
          text -> "_" <> text
      attachment = typeName <> ownerIdText <> "_" <> objIdText <> accessKey
   in do
    configHandle <- getApp
    liftIO . debugM (hLog configHandle) $ "Vk attachment: " <> T.unpack attachment
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
  let typeName = fromString $ getValue ["type"] attachmentObj
  let attachmentInfoObj = fromObject $ getValue [typeName] attachmentObj
  liftIO . debugM logHandle $ "Find vk attachments: " <> T.unpack typeName
  case typeName of
    "sticker" -> handleSticker typeName attachmentInfoObj
    "photo" -> handlePhoto typeName attachmentObj
    "doc" -> handleDoc typeName attachmentObj
    _ ->
      updateAttachment typeName attachmentInfoObj
  handleAttachments rest

handleSticker, handlePhoto, handleDoc :: TypeName -> Attachment -> App ()
handleSticker typeName attachmentObj =
  let field = "sticker_id"
      value = getValue [field] attachmentObj
   in modifyConfig
        (HM.insert "attachment" (A.String typeName) . HM.insert field value)

handlePhoto typeName attachmentObj = do
  let sizesGot = getValue [typeName, "sizes"] attachmentObj
  uploadPhotosServer (fromArrObject sizesGot)
  --let loop = do
  --      configHandle <- getApp
  --      responseObj <-
  --        fromObject . getValue ["response"] . fromObject <$>
  --        (tryHttpJson $ HTTPSimple.httpJSON =<< getPhotosGetReq configHandle)
  --      liftIO . debugM (hLog configHandle) $
  --        "photos.get object: " <> show responseObj
  --      let count = toInteger $ getValue ["count"] responseObj
  --      let offset = toInteger . getValue ["offset"] $ hConfig configHandle
  --      let sizesArrExpected =
  --            map (getValue ["sizes"]) . fromArrObject $
  --            getValue ["items"] responseObj
  --      if any (sizesGot ==) sizesArrExpected
  --        then updateAttachment typeName attachmentObj
  --        else if offset >= count || count <= 50
  --               then uploadPhotosServer (fromArrObject sizesGot) >>
  --                    updateAttachment typeName attachmentObj
  --               else modifyConfig (insertWithPush "offset" $ A.Number 50) >>
  --                    loop
  --loop

handleDoc = undefined

uploadPhotosServer :: [A.Object] -> App ()
uploadPhotosServer sizesObjArr = do
  let typeName = "photo"
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
  photoBS <-
    liftIO $
    HTTPClient.responseBody <$>
    (HTTPSimple.httpBS =<< HTTPClient.parseRequest url)
  photoPath <-
    liftIO $
    (\x -> x <> "\\Photo." <> (last . wordsBy (/= '.')) url) <$> getRepDir
  liftIO $ BS.writeFile photoPath photoBS
  configHandle1 <- getApp
  liftIO . debugM (hLog configHandle1) $ "Photo url: " <> url
  responseObj <-
    fromObject . getValue ["response"] . fromObject <$>
    (tryHttpJson $
     HTTPSimple.httpJSON =<< getPhotosGetMgsUploadServerReq configHandle1)
  let albumId = getValue ["album_id"] responseObj
  modifyConfig $ HM.insert "album_id" albumId
  liftIO . debugM (hLog configHandle1) $ "Upload object: " <> show responseObj
  let uploadUrl =
        HTTPClient.parseRequest_ . T.unpack . fromString $
        getValue ["upload_url"] responseObj
  let formData = HTTPClient.partFile typeName photoPath
  photosJson <-
    tryHttpJson $
    HTTPSimple.httpJSON =<< HTTPClient.formDataBody [formData] uploadUrl
  liftIO . debugM (hLog configHandle1) $ "Photo json: " <> show photosJson
  modifyConfig . HM.union $ HM.singleton "photo_obj" photosJson
  configHandle2 <- getApp
  photoObj <-
    head . fromArrObject . getValue ["response"] . fromObject <$>
    (tryHttpJson $
     HTTPSimple.httpJSON =<< getPhotosSaveMsgPhotoReq configHandle2)
  liftIO . debugM (hLog configHandle2) $ "Photo object: " <> show photoObj
  updateAttachment typeName photoObj

getMsg :: ObjApp Message
getMsg = do
  updates <- askSubApp
  let msg = fromString $ getValue ["object", "message", "text"] updates
  return msg
