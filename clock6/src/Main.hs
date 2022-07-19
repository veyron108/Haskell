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
import System.Console.ANSI as ANSI
import System.IO
import Data.Fixed
import System.Exit
import System.Directory.Internal.Prelude (exitFailure)

import BigNums


type App         = State ClockState
type Hours       = Int
type Minutes     = Int
type Seconds     = Int
type DayTime     = (Hours, Minutes, Seconds)

data ClockState = ClockState { 
    timeOfDay :: DayTime,                   
    offset    :: Seconds
  } deriving Show

data Event = SecondsEvent | KeyEvent Char deriving Show 

-------------------------------------------------------
-- Helpers
--
utcToUtc :: Int -> UTCTime -> UTCTime
utcToUtc offset = addUTCTime (realToFrac offset)                          -- changing this value is working fine

stringToInt :: String -> Int
stringToInt x = if all (`elem` "0123456789") x
                then read x :: Int
                else 0

splitTime :: String -> [String]
splitTime [] = [""]
splitTime (x : xs) | x == ':'  = "" : more
             | otherwise = (x : head more) : tail more
    where more = splitTime xs

-------------------------------------------------------
-- Clock State
--
getDayTime :: Int -> IO DayTime
getDayTime n = do
    now <- getCurrentTime                                     -- eg 2022-07-13 17:25:29.4547484 ->UTC
    timezone <- getCurrentTimeZone                            -- eg AEST
    let now'      = utcToUtc n now                            -- changed the offset
    let zoneNow   = utcToLocalTime timezone now'              -- eg 2022-07-14 03:24:41.7410000 ->Melb
    let timeOfDay = formatTime defaultTimeLocale "%H:%M:%S" zoneNow 
    let [h, m, s] = splitTime timeOfDay
    let hours     = stringToInt h
    let minutes   = stringToInt m
    let seconds   = stringToInt s
    return (hours, minutes, seconds)

initClockState :: IO ClockState 
initClockState = do 
  dt <- getDayTime 0
  return $ ClockState { timeOfDay = dt, offset = 0 }

getAwst :: State Int String
getAwst = do                            -- AWST
    current <- get                      -- Get the current state (our offset)
    put (- 7200)                        -- Update the state 
    return (show current)               -- Produce result

getAcst :: State Int String
getAcst = do                            -- ACST
    current <- get                      -- 
    put (- 1800)                        --  
    return (show current)               -- 

getAest :: State Int String
getAest = do                            -- AEST 
    current <- get                      -- 
    put 0                               -- 
    return (show current)               -- 

getClockStateOffset :: ClockState -> Int
getClockStateOffset ClockState { timeOfDay = b, offset = c } = c

-------------------------------------------------------
-- Clock Rendering
--
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
    return $ drawClock (timeOfDay cs) 
    --return $ drawClock (getClockStateTime cs)         -- no longer need this getter

-------------------------------------------------------
-- Events
--
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

-------------------------------------------------------
-- MAIN
--
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
  putStrLn "Press 'a' to change TimeZone to NZST - Auckland NewZeland"
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

  chan <- newChan               -- Channels and Events - see last class with fork.hs
  forkIO $ ticker chan 
  forkIO $ input chan

  forever $ do 
    c <- readChan chan
    case c of 

      -------- TICKER --------
      SecondsEvent  -> do

        let offset = 0            -- TODO : need to "get" offset value out of ClockState 
                                  -- if I manually type a value here it works       
        setCursorPosition 0 0
        dt <- getDayTime offset
        -- let theClockState = ClockState { timeOfDay = dt, offset = offset }
        -- putStrLn $ evalState drawClockState theClockState 
        let theClockState = ClockState { timeOfDay = dt, offset = offset }
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
          ANSI.showCursor
          clearFromCursorToScreenEnd
          exitFailure

        'e' -> do 
          -- TODO : need to "set" offset in ClockState to 0
          return ()
          
        'w' -> do 
          -- TODO : need to "set" offset in ClockState to (-7200)
          return ()

        'c' -> do 
          -- TODO : need to "set" offset in ClockState to (-1800)
          return ()

        'a' -> do 
          -- TODO : need to "set" offset in ClockState to (43200) - Pacific Auckland NZST
          return ()

        _   -> do 
          return ()

