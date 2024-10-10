{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Config 
  ( GlobalConfig(..)
  , LabelConfig(..)
  , SpriteSheetConfig(..)
  , SpriteConfig(..)
  ) where

import Data.Aeson 
import Data.Aeson.Types
import Control.Monad (foldM)
import Data.Functor ((<&>))

import Data.Map

import Game.Enums (Sound(..))

data GlobalConfig = GlobalConfig
  { cfgLevels  :: [FilePath]
  , cfgSounds  :: Map Sound FilePath
  , cfgSprites :: SpriteSheetConfig
  , cfgFont    :: FilePath 
  , cfgLabels  :: LabelConfig
  } deriving (Eq, Show)

instance FromJSON GlobalConfig where 
    parseJSON = withObject "global config" $ \obj -> 
        GlobalConfig
            <$> obj .: "levels"
            <*> (obj .: "sounds" >>= parseSounds)
            <*> obj .: "sprites"
            <*> obj .: "font"
            <*> obj .: "labels"


parseSounds :: Value -> Parser (Map Sound String)
parseSounds = 
    withObject "sounds" $ \obj -> do
        let insertSound acc sound = 
                obj .: key sound <&> \case 
                    Just path -> insert sound path acc
                    Nothing   -> acc
        foldM insertSound empty [minBound..]
  where 
    key = \case
        START       -> "start"
        SIREN       -> "siren"
        FRIGHTENED  -> "frightend"
        RETREATING  -> "retreating"
        DEATH_1     -> "death-1"
        DEATH_2     -> "death-2"
        EAT_POINT_1 -> "eat-point-1"
        EAT_POINT_2 -> "eat-point-2"
        EAT_FRUIT   -> "eat-fruit"
        EAT_GHOST   -> "eat-ghost"


data LabelConfig = LabelConfig
  { lbGameOver  :: String  -- ^ displayed on game over
  , lbStarting  :: String  -- ^ displayed when starting new level
  , lbRespawn   :: String  -- ^ displayed when respawning pacman
  , lbScore     :: String  -- ^ displayed in front of score
  , lbLevel     :: String  -- ^ displayed in front of level name
  } deriving (Eq, Show)

instance FromJSON LabelConfig where 
    parseJSON = withObject "level" $ \obj -> 
        LabelConfig
            <$> obj .: "game-over"
            <*> obj .: "starting"
            <*> obj .: "respawn"
            <*> obj .: "score"
            <*> obj .: "level-name"


data SpriteSheetConfig = SpriteSheetConfig
  { sprtSheet   :: String
  , sprtPoint   :: SpriteConfig
  , sprtReverse :: SpriteConfig
  , sprtFruit   :: SpriteConfig
  , sprtLife    :: SpriteConfig
  , sprtMuted   :: SpriteConfig
  , sprtUnmted  :: SpriteConfig
  } deriving (Eq, Show)

instance FromJSON SpriteSheetConfig where 
    parseJSON = withObject "sprites" $ \obj -> 
        SpriteSheetConfig
            <$> obj .: "sheet"
            <*> obj .: "point"
            <*> obj .: "reverse"
            <*> obj .: "fruit"
            <*> obj .: "life"
            <*> obj .: "muted"
            <*> obj .: "unmuted"

data SpriteConfig = SpriteConfig
  { sprtX      :: Float 
  , sprtY      :: Float
  , sprtWidth  :: Float
  , sprtHeight :: Float
  , sprtScale  :: Float
  } deriving (Eq, Show)

instance FromJSON SpriteConfig where 
    parseJSON = withObject "sprite" $ \obj -> 
        SpriteConfig
            <$> obj .: "x"
            <*> obj .: "y"
            <*> obj .: "width"
            <*> obj .: "height"
            <*> obj .: "scale"