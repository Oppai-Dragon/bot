module Bot.Telegram
    ( getKeysTelegram
    ) where


getKeysTelegram :: Object -> ReaderT Bot (StateT Config IO) Object
getKeysTelegram obj =
    let
        updateId = getValue ["update_id"] obj
        chatId = getValue ["message","chat","id"] obj
    in return $ HM.fromList [("offset",updateId),("chat_id",chatId)]

updateTelegram :: ReaderT (Object,Bot) (StateT Config IO) Message
updateTelegram = do
    updateKeys
    getTelegramMsg

updateKeys :: ReaderT (Object,Bot) (StateT Config IO) ()
updateKeys = do
    config <- lift $ get
    (updates,bot) <- ask
    let updateIdOld = getValue ["offset"] config
    let updateIdNew = getValue ["update_id"] updates
    let chatId = getValue ["message","chat","id"] updates
    let isSendMsg = Bool $ (/=) updateIdOld updateIdNew
    let localConfig = HM.fromList [("offset",updateIdNew),("chat_id",chatId),("isSendMsg",isSendMsg)]
    lift $ put $ HM.union localConfig config

getTelegramMsg :: ReaderT (Object,Bot) (StateT Config IO) Message
getTelegramMsg = do
    (updates,bot) <- ask
    let (String msg) = getValue ["message","text"] updates
    return msg