module Main where

import Control.Concurrent
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import System.Console.ANSI
import System.IO
import Data.Fixed
import System.Exit
import System.Directory.Internal.Prelude (exitFailure)


--------------------------------------------
-- NUMBERS

bigNum :: Int -> [String]
bigNum 0 =
    [" .d8888b.  ",
     "d88P  Y88b ",
     "888    888 ",
     "888    888 ",
     "888    888 ",
     "888    888 ",
     "Y88b  d88P ",
     " ^Y8888P^  "]
bigNum 1 =
    ["   d888    ",
     "  d8888    ",
     "    888    ",
     "    888    ",
     "    888    ",
     "    888    ",
     "    888    ",
     "  8888888  "]  
bigNum 2 =
    [" .d8888b.  ",
     "d88P  Y88b ",
     "888    888 ",
     "    .d88P  ",
     ".od888P^   ",
     "d88P^      ",
     "888^       ",
     "8888888888 "]
bigNum 3 =
    [" .d8888b.  ",
     "d88P  Y88b ",
     "     .d88P ",
     "    8888^  ",
     "     ^Y8b. ",
     "888    888 ",
     "Y88b  d88P ",
     " ^Y888P^   "]
bigNum 4 =
    ["    d8888  ",
     "   d8P888  ",
     "  d8P 888  ",
     " d8P  888  ",
     "d8P   888  ",
     "8888888888 ",
     "      888  ",
     "      888  "]
bigNum 5 =
    ["888888888  ",
     "888        ",
     "888        ",
     "8888888b.  ",
     "     ^Y88b ",
     "       888 ",
     "Y88b  d88P ",
     " ^Y8888P^  "]
bigNum 6 =
    [" .d8888b.  ",
     "d88P  Y88b ",
     "888        ",
     "8888888b.  ",
     "888P ^Y88b ",
     "888    888 ",
     "Y88b  d88P ",
     " ^Y8888P^  "]
bigNum 7 =
    ["8888888888 ",
     "      d88P ",
     "      d88P ",
     "    d88P   ",
     "   d88P    ",
     "  d88P     ",
     " dBBP      ",
     " dBBP      "]
bigNum 8 =
    [" .d8888b.  ",
     "d88P  Y88b ",
     "Y88b. d88P ",
     " ^Y88888^  ",
     ".d8P^^Y8b. ",
     "888    888 ",
     "Y88b  d88P ",
     " ^Y8888P^  "]
bigNum 9 =
    [" .d8888b.  ",
     "d88P  Y88b ",
     "888    888 ",
     "Y88b. d888 ",
     " ^Y888P888 ",
     "       888 ",
     "Y88b  d88P ",
     " ^Y8888P^  "]
bigNum _ = 
    ["          ",
     "          ",
     "          ",
     "          ",
     "          ",
     "          ",
     "          ",
     "          "]

starter :: [String]
starter = ["",
           "",
           "",
           "",
           "",
           "",
           "",
           ""]
          
spacer :: [String]
spacer = [" ",
          " ",
          " ",
          " ",
          " ",
          " ",
          " ",
          " "]

colonOn :: [String]
colonOn  = ["        ",
            "  d88b  ",
            "  Y88P  ",
            "        ",
            "        ",
            "  d8Bb  ",
            "  Y88P  ",
            "        "]

eol :: [String]
eol = ["\n","\n","\n","\n","\n","\n","\n","\n"]

--------------------------------------------
-- TYPES
type App         = State ClockState
type Hours       = Int
type Minutes     = Int
type Seconds     = Int
type DayTime     = (Hours, Minutes, Seconds)

data ClockState = ClockState { 
    timeOfDay :: (Int, Int, Int), 
    offset    :: Int
  } deriving Show

data Event = SecondsEvent | KeyEvent Char deriving Show 

--------------------------------------------
-- GENERAL FUNCTIONS
utcToUtc :: UTCTime -> UTCTime
utcToUtc = addUTCTime (realToFrac offset)  
  where offset = 0                          -- changing this value is working fine

stringToInt :: String -> Int
stringToInt x = if all (`elem` "0123456789") x
                then read x :: Int
                else 0

splitTime :: String -> [String]
splitTime [] = [""]
splitTime (x : xs) | x == ':'  = "" : more
             | otherwise = (x : head more) : tail more
    where more = splitTime xs

--------------------------------------------
-- CLOCK FUNCTIONS
getDayTime :: IO DayTime
getDayTime = do
    now <- getCurrentTime                                     -- eg 2022-07-13 17:25:29.4547484 ->UTC
    let now' = utcToUtc now                                   -- changed the offset
    timezone <- getCurrentTimeZone                            -- eg AEST
    let zoneNow   = utcToLocalTime timezone now'              -- eg 2022-07-14 03:24:41.7410000 ->Melb
    let timeOfDay = formatTime defaultTimeLocale "%H:%M:%S" zoneNow 
    let [h, m, s] = splitTime timeOfDay
    let hours     = stringToInt h
    let minutes   = stringToInt m
    let seconds   = stringToInt s
    return (hours, minutes, seconds)

initClockState :: IO ClockState 
initClockState = do 
  dt <- getDayTime
  return $ ClockState { timeOfDay = dt, offset = 0 }

getAwst :: State Int String
getAwst = do                            -- AWST
    current <- get                      -- Get the current state (our offset)
    put (- 7200)                        -- Update the state 
    return (show current)               -- Produce result

getAcst :: State Int String
getAcst = do                            -- ACST
    current <- get                      -- Get the current state (our offset)
    put (- 1800)                        -- Update the state 
    return (show current)               -- Produce result

getAest :: State Int String
getAest = do                            -- AEST 
    current <- get                      -- Get the current state (our offset)
    put 0                               -- Update the state 
    return (show current)               -- Produce result

getClockStateOffset :: ClockState -> Int
getClockStateOffset ClockState { timeOfDay = b, offset = c } = c

-- newtype State a b = State (a -> (a, b))

-- runState :: State a b -> a -> (a, b)
-- runState (State f) = f

-- evalState :: State a b -> a -> b
-- evalState (State f) = fst . f

-- execState :: State a b -> a -> b
-- execState (State f) = snd . f

-- get :: State a a
-- get = State $ \state -> (state, state)

-- put :: a -> State a ()
-- put newstate = State $ \_ -> (newstate, ())

-- clockOffsetState :: State ClockState Seconds
-- clockOffsetState = do 
--   cs@ClockState { offset = offset } <- get
--   let ns = offset
--   put (cs { offset = ns })
--   return ns

-- clockOffset :: State ClockState ()
-- clockOffset = do
--     let x = 3600
--     current <- get
--     put current { offset = x }
--     return ()

-- clockOffset' :: ClockState -> (ClockState, Int)
-- clockOffset' cs@ClockState{ offset = offset } = ( cs{offset = 3600}, 3600 )

-- updateOffset :: Seconds -> State ClockState ()
-- updateOffset s = modify (offset s)

-- clockDayTime' :: ClockState -> (ClockState, Int)
-- clockDayTime' cs@ClockState{ timeOfDay = timeOfDay } = 
--   let 
--     ns = timeOfDay :: DayTime
--   in 
--     ( cs{timeOfDay = ns}, ns)

-- update :: ClockState -> ClockState
-- update = evalState clockOffset

--------------------------------------------
-- RENDER FUNCTIONS

drawVAnim :: Int -> [String] -> [String]                                  -- NOT IN USE
drawVAnim 0 [] = []
drawVAnim n (x : xs) 
  | n > 0 = x : drawVAnim (n - 1) xs 
  | otherwise  = []
drawVAnim _ _  = [] 

doubleDigits :: Int -> [[String]]
doubleDigits n = uncurry (++) ([bigNum a], [bigNum b])                    -- [Strings] ++ [Strings] ++ [Strings] also works
    where 
      (a, b) = n `divMod` 10                                              -- calc the 10's and 1's

convertNestedListToString :: [[String]] -> String
convertNestedListToString n = concat $ concat n

-- Manipulate the List of Strings so they appear horizontally by transposing
-- Notes: Changed from own function to using transpose - forgot that was available earlier - doh!

-- NOT IN USE
-- colToRows :: [[a]] -> [[a]]
-- colToRows ([]:_) = []
-- colToRows r = map head r : colToRows (map tail r)

drawClockString :: [[String]] -> String
drawClockString x = convertNestedListToString $ transpose x   -- use transpose x -- same thing!

drawClock :: DayTime -> String                                -- i.e. Render Clock String using DayTime tuple as argument 
drawClock (h, m, s) = drawClockString output 
  where  
    hours     = doubleDigits h                                          
    minutes   = doubleDigits m                                           
    seconds   = doubleDigits s  
    output    = join [hours, [colonOn], minutes, [colonOn], seconds, [eol]]  -- "join" the Lists // using Control.Monad                                            

drawClockState :: State ClockState String 
drawClockState =
  get >>= \cs ->
    --return $ drawClock (getClockStateTime cs)         -- no longer need this getter
    return $ drawClock (timeOfDay cs)  

--------------------------------------------
-- EVENTS
ticker :: Chan Event -> IO () 
ticker chan = forever $ do 
  threadDelay (10 ^ 6)                  -- every 1 second is 10^6 microseconds
  writeChan chan SecondsEvent

input :: Chan Event -> IO () 
input chan = forever $ do  
  hSetEcho stdin False
  c <- getChar 
  hSetEcho stdin True
  writeChan chan (KeyEvent c)

--------------------------------------------
-- MAIN
main :: IO ()
main = do

  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetEcho stdin False
  clearScreen
  hideCursor
  
  setCursorPosition 10 0
  setSGR [ SetColor Foreground Vivid White]
  putStrLn "===== INSTRUCTIONS ====="
  putStrLn "Press 'x' to quit"
  putStrLn "Press '1' to '3' to change to color"
  putStrLn "Press 'e' to change TimeZone to AEST - Melbourne, Sydney, Brisbane"
  putStrLn "Press 'w' to change TimeZone to AWST - Perth"
  putStrLn "Press 'c' to change TimeZone to ACST - Adelaide, Darwin"
  setSGR [ SetColor Foreground Vivid Green]
  
  ics <- initClockState

  setCursorPosition 0 0
  --print ics 

  -- NOT IN USE : Heading Animation
  -- forM_ [1..8] (\ i -> do                                            
  --   setCursorPosition 0 0 
  --   threadDelay (10 ^ 5)
  --   putStrLn $ concat $ drawVAnim i title
  --   )

  chan <- newChan                       -- Channels and Events - see last class with fork.hs
  forkIO $ ticker chan 
  forkIO $ input chan

  forever $ do 
    c <- readChan chan
    case c of 

      -------- TICKER --------
      SecondsEvent  -> do

        -- TODO : we only want to "change" timeOfDay tuple
        setCursorPosition 0 0
        dt <- getDayTime
        let theClockState = ClockState { timeOfDay = dt, offset = 0 }
        putStrLn $ evalState drawClockState theClockState 

      ------- KEY EVENTS -------
      KeyEvent c -> case c of 

        '1' -> do 
          setSGR [SetColor Foreground Vivid Green]
          return ()
        '2' -> do 
          setSGR [SetColor Foreground Vivid Red]
          return ()
        '3' -> do 
          setSGR [SetColor Foreground Vivid Yellow]
          return ()
        
        'x' -> do                                      
          putStrLn "Exit Clock"
          showCursor
          clearFromCursorToScreenEnd
          exitFailure

        'w' -> do 
          
          return ()

        _   -> do 
          return ()

