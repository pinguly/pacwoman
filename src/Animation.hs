module Animation 
  ( Animations(..)
  , animationProgress
  , BlinkAnimation(..)
  , newBlink
  ) where 

data Animations = Animations
  { animDotBlink   :: BlinkAnimation
  } deriving (Eq, Show)

animationProgress :: Float -> Animations -> Animations
animationProgress deltaTime anim =
    Animations
      { animDotBlink   = blinkProgress deltaTime (animDotBlink   anim)
      }

data BlinkAnimation = BlinkAnimation 
  { blinkOn        :: Bool
  , blinkRemaining :: Float
  , blinkInterval  :: Float
  } deriving (Eq, Show)

newBlink :: Float -> BlinkAnimation
newBlink interval = BlinkAnimation True interval interval

blinkProgress :: Float -> BlinkAnimation -> BlinkAnimation
blinkProgress deltaTime blink
    | deltaTime <= 0         = blink  
    | deltaTime <= remaining = BlinkAnimation state (remaining - deltaTime) interval
    | otherwise              = 
        blinkProgress (deltaTime - remaining) $
            BlinkAnimation (not state) interval interval
  where 
    state     = blinkOn blink
    remaining = blinkRemaining blink
    interval  = blinkInterval blink

