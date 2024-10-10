{-# LANGUAGE TypeFamilies #-}

module Sounding
 ( MonadAudio(..)
 , toggleMute
 ) where

class Monad m => MonadAudio m where
    type Sound m 

    playSound :: Sound m -> m ()
    stopSound :: Sound m -> m ()
    setMute   :: Bool -> m ()
    isMute    :: m Bool

toggleMute :: MonadAudio m => m ()
toggleMute = isMute >>= setMute . not