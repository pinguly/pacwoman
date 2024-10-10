{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module JS
  ( JSArray(..)
  , JSPromise(..)
  , JSString(..)
  , JSUndefined(..)
  , Float32Array(..)
  , HTMLImageElement(..)
  , toJSString
  , fromJSString
  , toJSArray
  , fromJSArray
  , toFloat32Array
  , JSIO
  , runJSIO
  , await
  , sleep
  , requestAnimationFrame
  , fetchText
  , fetchJSON
  , fetchImage
  , consoleLog
  , consoleError
  ) where


import Control.Applicative
import Control.Monad (MonadPlus, mzero, void, forM)
import Control.Monad.Cont (MonadCont, callCC)
import Control.Monad.Except (ExceptT(..), MonadError(..), runExceptT, liftEither)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Cont (ContT(..), evalContT)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import Data.Aeson (FromJSON, eitherDecodeStrict)
import Data.Coerce (Coercible, coerce)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.String (fromString)

import qualified GHC.JS.Prim
import GHC.JS.Prim (JSVal)
import GHC.JS.Foreign.Callback (Callback, OnBlocked(..), syncCallback, syncCallback1, syncCallback3, releaseCallback)


newtype JSArray a   = JSArray JSVal
newtype JSPromise a = JSPromise JSVal
newtype JSString    = JSString JSVal
newtype JSUndefined = JSUndefined JSVal

newtype Float32Array = Float32Array JSVal
newtype HTMLImageElement = HTMLImageElement JSVal



toJSString :: String -> JSString
toJSString = JSString . GHC.JS.Prim.toJSString

fromJSString :: JSString -> String
fromJSString (JSString string) = GHC.JS.Prim.fromJSString string


toJSArray :: Coercible a JSVal => [a] -> IO (JSArray a)
toJSArray = (coerce <$>) . GHC.JS.Prim.toJSArray . map coerce

fromJSArray :: Coercible JSVal a => JSArray a -> IO [a]
fromJSArray (JSArray array) = 
    map coerce <$> GHC.JS.Prim.fromJSArray array


foreign import javascript "(x => x)"
    js_float :: Float -> IO JSVal

foreign import javascript "(x => new Float32Array(x))"
    js_new_Float32Array :: JSVal -> IO JSVal

toFloat32Array :: [Float] -> IO Float32Array
toFloat32Array =
    (Float32Array <$>) . (js_new_Float32Array =<<) . (GHC.JS.Prim.toJSArray =<<) . mapM js_float



type JSIO = ExceptT String (ContT () IO)

runJSIO :: JSIO a -> IO ()
runJSIO context =
    evalContT $ void $ do
        runExceptT context >>= \case
            Left err -> liftIO $ js_console_error $ coerce $ toJSString err       
            Right _  -> return ()


foreign import javascript "console.log"
  js_console_log :: JSVal -> IO ()

foreign import javascript "console.error"
  js_console_error :: JSVal -> IO ()

consoleLog :: (MonadIO m, Show a) => a -> m ()
consoleLog = liftIO . js_console_log . coerce . toJSString . show

consoleError :: (MonadIO m, Show a) => a -> m ()
consoleError = liftIO . js_console_error . coerce . toJSString . show


foreign import javascript "(($1, $2, $3) => $1.then(x => $2(x, $2, $3), x => $3(x, $2, $3)))"
    js_promise_then :: JSPromise a
                    -> Callback (JSVal -> JSVal -> JSVal -> IO ()) 
                    -> Callback (JSVal -> JSVal -> JSVal -> IO ()) 
                    -> IO ()

foreign import javascript "(($1) => $1)"
    js_coerce_callback :: JSVal -> Callback a

await :: Coercible JSVal a => JSPromise a -> JSIO a
await promise = 
    ExceptT . fmap Right . ContT $ \next -> do
        success <- syncCallback3 ThrowWouldBlock $ \value cb1 cb2 -> do
                       releaseCallback $ js_coerce_callback cb1
                       releaseCallback $ js_coerce_callback cb2
                       next $ coerce value   
        failure <- syncCallback3 ThrowWouldBlock $ \err cb1 cb2 -> do
                       releaseCallback $ js_coerce_callback cb1
                       releaseCallback $ js_coerce_callback cb2
                       js_console_error err
        js_promise_then promise success failure


foreign import javascript "(($1, $2) => setTimeout($1, $2, $1))"
  js_setTimeout :: Callback (JSVal -> IO ()) -> Int -> IO ()

sleep :: Int -> JSIO ()
sleep duration = 
  ExceptT . fmap Right . ContT $ \next -> do
        callback <- syncCallback1 ThrowWouldBlock $ \cb -> do
                       releaseCallback $ js_coerce_callback cb
                       next ()   
        js_setTimeout callback duration


foreign import javascript "(($1) => window.requestAnimationFrame($1))"
    js_requestAnimationFrame :: Callback (IO ()) -> IO ()

requestAnimationFrame :: IO () -> IO ()
requestAnimationFrame action = do
    ref <- newIORef Nothing 
    callback <- syncCallback ThrowWouldBlock $ do 
                    readIORef ref >>= mapM_ releaseCallback
                    action
    writeIORef ref (Just callback)
    js_requestAnimationFrame callback


foreign import javascript "(($1) => loadFile($1))"
  js_loadFile :: JSString -> IO (JSPromise JSVal)

fetchText :: String -> JSIO String
fetchText = (fromJSString . coerce <$>) . (await =<<) . liftIO . js_loadFile . toJSString

fetchJSON :: FromJSON a => String -> JSIO a
fetchJSON = (liftEither . eitherDecodeStrict . fromString =<<) . fetchText


foreign import javascript "(($1) => {const img = new Image(); img.src = $1; return img.decode().then(() => img);})"
    js_loadImage :: JSString -> IO (JSPromise HTMLImageElement)

fetchImage :: String -> JSIO HTMLImageElement
fetchImage = (await =<<) . liftIO . js_loadImage . coerce . toJSString