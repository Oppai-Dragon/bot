module Bot.Vk
    ( getKeysVk
    ) where

getKeysVk :: Object -> ReaderT Bot (StateT Config IO) Object
getKeysVk = return

updateVk :: ReaderT (Object,Bot) (StateT Config IO) Message
updateVk = do
    updateTs
    getVkMsg

updateTs :: ReaderT (Object,Bot) (StateT Config IO) ()
updateTs = do
    config <- lift $ get
    (updates,bot) <- ask
    let ts = getValue ["ts"] updates
    let localConfig = HM.singleton "ts" ts
    lift $ put $ HM.union localConfig config

getVkMsg :: ReaderT (Object,Bot) (StateT Config IO) Message
getVkMsg = do
    (updates,bot) <- ask
    let (String msg) = getValue ["object","message","text"] updates
    return msg