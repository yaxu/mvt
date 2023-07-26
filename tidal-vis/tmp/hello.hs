
-- | Display "Hello World" in a window.


import           Control.Concurrent
import           Control.Concurrent.MVar
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Animate

makeThread :: IO (MVar String)
makeThread = do myVariable <- newMVar "Hello World!!!"
                putStr "a"
                threadId <- forkIO $ myAnimate myVariable
                putStr "b"
                return myVariable

myAnimate myVariable =
  animateIO
        (InWindow
               "Hello World!!"     -- window title
                (400, 150)       -- window size
                (10, 10))        -- window position
        white                    -- background color
        (picture myVariable)     -- picture to display
        controllerSetRedraw

picture :: MVar String -> Float -> IO Picture
picture myVariable t =
  do str <- readMVar myVariable
     return $ Translate (-170) (-20) -- shift the text to the middle of the window
       $ Scale 0.5 0.5               -- display it half the original size
       $ Text (str ++ " " ++ show t)
