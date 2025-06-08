module Command.Cat.CatSet.Parser where

import External.ChatAPI hiding (SystemMessage)
import MeowBot
import MeowBot.Parser
import qualified Data.Text as T
import qualified MeowBot.Parser as MP

data CatSetCommand
  = Set   DefaultOrPerChat BotSettingItem
  | UnSet DefaultOrPerChat BotSettingItem
  | View  DefaultOrPerChat BotSettingItem
  | Clear DefaultOrPerChat
  deriving (Show, Eq)

data DefaultOrPerChat = Default | PerChat | PerChatWithChatId ChatId deriving (Show, Eq)

data BotSettingItem
  = DisplayThinking         (Maybe Bool)
  | DisplayToolMessage      (Maybe Bool)
  | DefaultModel            (Maybe ChatModel)
  | DefaultModelSuper       (Maybe ChatModel)
  | SystemMessage           (Maybe Text)
  | SystemTemp              (Maybe Double)
  | SystemMaxToolDepth      (Maybe Int)
  | SystemAPIKeyOpenAI      (Maybe Text)
  | SystemAPIKeyDeepSeek    (Maybe Text)
  | SystemAPIKeyOpenRouter  (Maybe Text)
  | SystemAPIKeySiliconFlow (Maybe Text)
  | ActiveChat              (Maybe Bool)
  | AtReply                 (Maybe Bool)
  | MentionReply            (Maybe Bool)
  | ActiveProbability       (Maybe Double)
  | MaxMessageInState       (Maybe Int)
  | Note                    (Maybe Int) -- ^ note id
  | CronTab                 (Maybe Int) -- ^ cron tab id
  | EnableNotes             (Maybe Bool) -- ^ enable notes
  | EnableCronTab           (Maybe Bool) -- ^ enable cron tab
  deriving (Show, Eq)

catSetParser :: Parser T.Text Char CatSetCommand
catSetParser =
  (MP.headCommand "cat-" >> do
    action <- asum
      [ MP.string "set"   >> return Set
      , MP.string "unset" >> return UnSet
      , MP.string "view"  >> return View
      ]
    MP.spaces
    range <- asum
      [ MP.string "default" *> return Default  <* MP.spaces
      , MP.string "perchat" *> MP.spaces
            *> (PerChatWithChatId <$> chatIdP) <* MP.spaces
      , MP.string "perchat" *> return PerChat  <* MP.spaces
      , return PerChat
      ]
    return $ action range (DisplayThinking Nothing)
    asum
      [ MP.string "displayThinking"         >> fmap (action range) (DisplayThinking         <$> MP.optMaybe (MP.spaces >> MP.bool))
      , MP.string "displayToolMessage"      >> fmap (action range) (DisplayToolMessage      <$> MP.optMaybe (MP.spaces >> MP.bool))
      , MP.string "defaultModelSuper"       >> fmap (action range) (DefaultModelSuper       <$> MP.optMaybe (MP.spaces >> MP.parseByRead))
      , MP.string "defaultModel"            >> fmap (action range) (DefaultModel            <$> MP.optMaybe (MP.spaces >> MP.parseByRead))
      , MP.string "systemMessage"           >> fmap (action range) (SystemMessage           <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
      , MP.string "systemTemp"              >> fmap (action range) (SystemTemp              <$> MP.optMaybe (MP.spaces >> MP.nFloat))
      , MP.string "systemMaxToolDepth"      >> fmap (action range) (SystemMaxToolDepth      <$> MP.optMaybe (MP.spaces >> MP.intRange 1 10))
      , MP.string "systemAPIKeyOpenAI"      >> fmap (action range) (SystemAPIKeyOpenAI      <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
      , MP.string "systemAPIkeyDeepSeek"    >> fmap (action range) (SystemAPIKeyDeepSeek    <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
      , MP.string "systemAPIKeyOpenRouter"  >> fmap (action range) (SystemAPIKeyOpenRouter  <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
      , MP.string "systemAPIKeySiliconFlow" >> fmap (action range) (SystemAPIKeySiliconFlow <$> MP.optMaybe (MP.spaces >> MP.some' MP.item))
      , MP.string "activeChat"              >> fmap (action range) (ActiveChat              <$> MP.optMaybe (MP.spaces >> MP.bool))
      , MP.string "atReply"                 >> fmap (action range) (AtReply                 <$> MP.optMaybe (MP.spaces >> MP.bool))
      , MP.string "mentionReply"            >> fmap (action range) (MentionReply            <$> MP.optMaybe (MP.spaces >> MP.bool))
      , MP.string "activeProbability"       >> fmap (action range) (ActiveProbability       <$> MP.optMaybe (MP.spaces >> require (\x -> x <= 0.20 && x >= 0) MP.nFloat))
      , MP.string "maxMessageInState"       >> fmap (action range) (MaxMessageInState       <$> MP.optMaybe (MP.spaces >> MP.intRange 1 24))
      , MP.string "note"                   >> fmap (action range) (Note <$> MP.optMaybe (MP.spaces >> MP.int))
      , MP.string "crontab"                >> fmap (action range) (CronTab <$> MP.optMaybe (MP.spaces >> MP.int))
      , MP.string "enableNotes"            >> fmap (action range) (EnableNotes <$> MP.optMaybe (MP.spaces >> MP.bool))
      , MP.string "enableCronTab"          >> fmap (action range) (EnableCronTab <$> MP.optMaybe (MP.spaces >> MP.bool))
      ]
    ) <|> (MP.headCommand "cat-clear" >> Clear <$> asum
            [ MP.spaces >> MP.string "default"  *> return Default
            , MP.spaces >> MP.string "perchat"  *> MP.spaces *> (PerChatWithChatId <$> chatIdP)
            , MP.spaces >> MP.string "perchat"  *> return PerChat
            , return PerChat
            ])
  where chatIdP = asum
          [ MP.string "user"  >> MP.spaces >> PrivateChat . UserId  <$> MP.int
          , MP.string "group" >> MP.spaces >> GroupChat   . GroupId <$> MP.int
          ]

unitTestsCatSetParser :: [(String, Bool)]
unitTestsCatSetParser =
  [ (":cat-clear -> Clear PerChat", MP.runParser catSetParser ":cat-clear" == Just (Clear PerChat))
  , (":cat-clear perchat group 123 -> Clear PerChatWithChatId (GroupChat (GroupId 123))"
    , MP.runParser catSetParser ":cat-clear perchat group 123" == Just (Clear (PerChatWithChatId (GroupChat (GroupId 123))))
    )
  , (":cat-set default displayThinking true -> Set Default (DisplayThinking (Just True))"
    , MP.runParser catSetParser ":cat-set default displayThinking true" == Just (Set Default (DisplayThinking (Just True)))
    )
  ]
