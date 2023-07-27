
-- | Display "Hello World" in a window.


import           Control.Concurrent
import           Control.Concurrent.MVar
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Animate
import qualified Sound.Tidal.Context                 as T

data Turtle = L Float
            | R Float
            | F Float

makeThread :: IO (MVar (T.Pattern Turtle))
makeThread = do mvPattern <- newMVar $ T.fastcat $ map pure [F 10, R 45, F 20, L 90, F 10]
                putStr "a"
                threadId <- forkIO $ myAnimate mvPattern
                putStr "b"
                return mvPattern

myAnimate mvPattern =
  animateIO
        (InWindow
               "Hello World!!"   -- window title
                (400, 150)       -- window size
                (10, 10))        -- window position
        white                    -- background color
        (picture mvPattern)     -- picture to display
        controllerSetRedraw

drawLines :: [Turtle] -> Picture
drawLines []         = mempty
drawLines ((L angle):ts) = rotate angle $ drawLines ts
drawLines ((R angle):ts) = rotate (0 - angle) $ drawLines ts
drawLines ((F len):ts) = translate len 0 $ pictures [line [(0,0), (-len,0)], drawLines ts]

picture :: MVar (T.Pattern Turtle) -> Float -> IO Picture
picture mvPattern t =
  do pat <- readMVar mvPattern
     let values = map T.value $ T.queryArc pat (T.Arc (toRational t) (toRational $ t + 4))
     return -- Translate (-170) (-20) -- shift the text to the middle of the window
       -- $ Scale 0.5 0.5               -- display it half the original size
       $ drawLines values
