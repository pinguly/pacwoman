{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Audio 
  ( WebAudioContext
  , WebSound
  , initWebAudio
  , loadWebSound
  ) where 

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, ask)
import Data.Coerce (coerce)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import GHC.JS.Prim (JSVal, getProp)

import Sounding (MonadAudio(..))
import JS (JSIO, JSPromise, JSString, toJSString, await)

newtype JSAudioContext = JSAudioContext JSVal
newtype JSAudioBuffer  = JSAudioBuffer JSVal
newtype JSGainNode     = JSGainNode JSVal
newtype JSAudioBufferSourceNode = JSAudioBufferSourceNode JSVal
newtype JSAudioDestinationNode  = AudioDestinationNode JSVal

foreign import javascript "(($1) => new AudioContext())"
    js_newAudioContext :: IO JSAudioContext

foreign import javascript "(($1) => $1.createGain())"
    js_createGain :: JSAudioContext -> IO JSGainNode

foreign import javascript "(($1, $2) => $1.connect($2))"
    js_connect :: JSVal -> JSVal -> IO ()

foreign import javascript "(($1, $2, $3) => {const s = $1.createBufferSource(); s.buffer = $2; s.loop = $3; return s;})"
    js_createBufferSource :: JSAudioContext -> JSAudioBuffer -> Bool -> IO JSAudioBufferSourceNode

foreign import javascript "(($1) => $1.start())"
    js_start :: JSAudioBufferSourceNode -> IO ()

foreign import javascript "(($1) => $1.stop())"
    js_stop :: JSAudioBufferSourceNode -> IO ()

foreign import javascript "(($1, $2) => {$1.gain.value = $2;})"
    js_gain_value :: JSGainNode -> Float -> IO ()

foreign import javascript "(($1, $2) => window.loadAudio($1, $2))"
    js_loadAudio :: JSAudioContext -> JSString -> IO (JSPromise JSAudioBuffer)


data WebSound = WebSound 
  { wsBuffer  :: JSAudioBuffer
  , wsLooping :: Bool
  , wsSource  :: IORef (Maybe JSAudioBufferSourceNode)
  }

data WebAudioContext = WebAudioContext
  { waAudio    :: JSAudioContext
  , waGainNode :: JSGainNode
  , waMuted    :: IORef Bool
  }

initWebAudio :: IO WebAudioContext
initWebAudio = do
    audio <- js_newAudioContext
    gain  <- js_createGain audio
    js_gain_value gain 0
    dst   <- getProp (coerce audio) "destination"
    js_connect (coerce gain) dst
    WebAudioContext audio gain <$> newIORef True


fetchAudio :: JSAudioContext -> String -> JSIO JSAudioBuffer
fetchAudio ctx = (await =<<) . liftIO . js_loadAudio ctx . coerce . toJSString

loadWebSound :: WebAudioContext  -> Bool -> String -> JSIO WebSound
loadWebSound ctx loop name = do 
    buffer <- fetchAudio (waAudio ctx) name
    liftIO $
        WebSound buffer loop <$> newIORef Nothing


instance MonadAudio (ReaderT WebAudioContext IO) where 

    type Sound (ReaderT WebAudioContext IO) = WebSound

    playSound sound = do
        ctx <- ask
        liftIO $ do
            readIORef (wsSource sound) >>= mapM_ js_stop
            source <- js_createBufferSource (waAudio ctx) (wsBuffer sound) (wsLooping sound)
            writeIORef (wsSource sound) $ Just source
            js_connect (coerce source) (coerce (waGainNode ctx))
            js_start source

    stopSound sound = do
        liftIO (readIORef (wsSource sound)) >>= mapM_ (liftIO . js_stop)

    setMute mute = do
        ctx <- ask
        liftIO $ do
            js_gain_value (waGainNode ctx) gain
            writeIORef (waMuted ctx) mute
      where
        gain = if mute then 0 else 1

    isMute =
        liftIO . readIORef =<< waMuted <$> ask