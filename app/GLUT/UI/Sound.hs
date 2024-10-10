{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module UI.Sound
  ( ALSound(..)
  , ALContext(..)
  , defaultALContext
  , loadALSound
  ) where

import Sounding

import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Sound.OpenAL
import Sound.ALUT
import Data.IORef (newIORef, IORef, readIORef, modifyIORef)


data ALSound = ALSound
  { alBuffer :: Buffer
  , alSource :: Source
  }

data ALContext = ALContext
  { alSources :: IORef [Source]
  , alMuted   :: IORef Bool
  }

defaultALContext ::IO ALContext
defaultALContext = 
    ALContext <$> newIORef []
              <*> newIORef False


-- TODO: MonadFail
loadALSound :: ALContext -> Bool -> FilePath -> IO ALSound
loadALSound ctx looping path = do
    buffer <- liftIO . createBuffer $ File path
    source <- genObjectName
    modifyIORef (alSources ctx) (source :)
    loopingMode source $= if looping then Looping else OneShot
    return $ ALSound buffer source


instance MonadAudio (ReaderT ALContext IO) where 

    type Sound (ReaderT ALContext IO) = ALSound

    playSound sound = 
        liftIO $ do
            stop [source]
            buffer source $= Just (alBuffer sound)
            play [source]
      where 
        source = alSource sound

    stopSound sound =
        liftIO $ do
            stop [source]
            buffer source $= Nothing
      where 
        source = alSource sound 

    setMute mute = do
        ctx <- ask
        liftIO $ do
            alMuted ctx $= mute
            sources <- readIORef $ alSources ctx
            forM_ sources $ 
                ($= value) . sourceGain 
      where   
        value = if mute then 0 else 1

    isMute = 
        liftIO . readIORef . alMuted =<< ask
