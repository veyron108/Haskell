{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Concurrent
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Control.Monad
import Data.List
import Data.List.Split
import Data.Time.LocalTime
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import System.Console.ANSI
import System.IO
import Data.Fixed
import System.Exit
import System.Directory.Internal.Prelude (exitFailure)
import GHC.Base (VecElem(Int16ElemRep))



-------------------------------------------------------------------
-- NUMBER STRINGS
-- bigNum is 10 col x 8 row characters
bigNum :: Int -> [String]
bigNum 0 =
    [" .d8888b. ",
     "d88P  Y88b",
     "888    888",
     "888    888",
     "888    888",
     "888    888",
     "Y88b  d88P",
     " ^Y8888P^ "]
bigNum 1 =
    ["   d888   ",
     "  d8888   ",
     "    888   ",
     "    888   ",
     "    888   ",
     "    888   ",
     "    888   ",
     "  8888888 "]  
bigNum 2 =
    [" .d8888b. ",
     "d88P  Y88b",
     "888    888",
     "    .d88P ",
     ".od888P^  ",
     "d88P^     ",
     "888^      ",
     "8888888888"]
bigNum 3 =
    [" .d8888b. ",
     "d88P  Y88b",
     "     .d88P",
     "    8888^ ",
     "     ^Y8b.",
     "888    888",
     "Y88b  d88P",
     " ^Y888P^  "]
bigNum 4 =
    ["    d8888 ",
     "   d8P888 ",
     "  d8P 888 ",
     " d8P  888 ",
     "d8P   888 ",
     "8888888888",
     "      888 ",
     "      888 "]
bigNum 5 =
    ["888888888 ",
     "888       ",
     "888       ",
     "8888888b. ",
     "     ^Y88b",
     "       888",
     "Y88b  d88P",
     " ^Y8888P^ "]
bigNum 6 =
    [" .d8888b. ",
     "d88P  Y88b",
     "888       ",
     "8888888b. ",
     "888P ^Y88b",
     "888    888",
     "Y88b  d88P",
     " ^Y8888P^ "]
bigNum 7 =
    ["8888888888",
     "      d88P",
     "      d88P",
     "    d88P  ",
     "   d88P   ",
     "  d88P    ",
     " dBBP     ",
     " dBBP     "]
bigNum 8 =
    [" .d8888b. ",
     "d88P  Y88b",
     "Y88b. d88P",
     " ^Y88888^ ",
     ".d8P^^Y8b.",
     "888    888",
     "Y88b  d88P",
     " ^Y8888P^ "]
bigNum 9 =
    [" .d8888b. ",
     "d88P  Y88b",
     "888    888",
     "Y88b. d888",
     " ^Y888P888",
     "       888",
     "Y88b  d88P",
     " ^Y8888P^ "]
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
colonOn  = ["          ",
            "   d88b   ",
            "   Y88P   ",
            "          ",
            "          ",
            "   d8Bb   ",
            "   Y88P   ",
            "          "]

colonOff :: [String]
colonOff = ["          ",
            "          ",
            "          ",
            "          ",
            "          ",
            "          ",
            "          ",
            "          "]

-- must have a \n at the end of each row on the final time string
eol :: [String]
eol = ["\n","\n","\n","\n","\n","\n","\n","\n"]


title :: [String]
title = ["888    888                   888               888 888       .d8888b.  888                   888      \n" 
        ,"888    888                   888               888 888      d88P  Y88b 888                   888      \n" 
        ,"888    888                   888               888 888      888    888 888                   888      \n" 
        ,"8888888888  8888b.  .d8888b  888  888  .d88b.  888 888      888        888  .d88b.   .d8888b 888  888 \n" 
        ,"888    888     ^88b 88K      888 .88P d8P  Y8b 888 888      888        888 d88^^88b d88P^    888 .88P \n" 
        ,"888    888 .d888888 ^Y8888b. 888888K  88888888 888 888      888    888 888 888  888 888      888888K  \n" 
        ,"888    888 888  888      X88 888 ^88b Y8b.     888 888      Y88b  d88P 888 Y88..88P Y88b.    888 ^88b \n"
        ,"888    888 ^Y888888  88888P' 888  888  ^Y8888  888 888       ^Y8888P^  888  ^Y88P^   ^Y8888P 888  888 \n"]


-------------------------------------------------------------------
-- DATA TYPES

type Hours       = Int
type Minutes     = Int
type DayTime     = (Int, Int, Int)
type App         = State ClockState

data ClockState = ClockState { 
  asList :: [String], 
  timeOfDay :: DayTime,
  offset :: Int
} deriving Show


-- Animate Vertical a List of Strings
drawVAnim :: Int -> [String] -> [String]
drawVAnim 0 [] = []
drawVAnim n (x : xs) 
  | n > 0 = x : drawVAnim (n - 1) xs 
  | otherwise  = []
drawVAnim _ _  = [] 


-- NOT USED
-- Repeat IO action -- redundant as I could probably can use replicateM_ ?
repeatIOAction :: Int -> IO() -> IO()
repeatIOAction 0 _ = return ()
repeatIOAction n thisAction =
 do
  thisAction
  repeatIOAction (n-1) thisAction


-- Get tuple of 10's and 1's and make List
doubleDigits :: Int -> [[String]]
doubleDigits n = [bigNum a] ++ [spacer] ++ [bigNum b]                          -- [Strings] ++ [Strings] ++ [Strings] also works
    where 
      (a, b) = n `divMod` 10                                                      -- divMod :: Integral a => a -> a -> (a, a)


-- Change nested List String to String
convertNestedListToString :: [[String]] -> String
convertNestedListToString n = concat $ concat n


-- Convert from IO ZonedTime to TimeOfDay
convertToTimeOfDay :: ZonedTime -> TimeOfDay                                                     
convertToTimeOfDay obj = localTimeOfDay localtime                                 -- localTimeOfDay :: TimeOfDay
  where 
    localtime = zonedTimeToLocalTime obj                                          -- zonedTimeToLocalTime :: LocalTime

  
-- Need to go from Data.Fixed.Pice E12 to Int
-- Easiest way is to just floor to the nearest Int (downwards)
-- Don't use round as sometimes "60" seconds is shown
convertPicoToInt :: Pico -> Int
convertPicoToInt = floor 


-- Manipulate the List of Strings so they appear horizontally by transposing
-- Notes: Changed from own function to using transpose - forgot that was available earlier - doh!

-- NOT USED
-- colToRows :: [[a]] -> [[a]]
-- colToRows ([]:_) = []
-- colToRows r = map head r : colToRows (map tail r)

drawClockString :: [[String]] -> String
drawClockString x = convertNestedListToString $ transpose x   -- use transpose x -- same thing!                   


-- i.e. Render Clock String using TimeOfDay as argument 
drawClock :: DayTime -> String
drawClock (h, m, s) = drawClockString output 
  where  
    hours     = doubleDigits h                                          
    minutes   = doubleDigits m                                           
    seconds   = doubleDigits s  
    output    = join [[starter], hours, [colonOn], minutes, [colonOn], seconds, [eol]]  -- "join" the Lists // using Control.Monad                                            


-- getters
getClockStateTime :: ClockState -> DayTime
getClockStateTime ClockState { asList = a, timeOfDay = b, offset = c } = b

getClockStateString :: ClockState -> [String]
getClockStateString ClockState { asList = a, timeOfDay = b, offset = c } = a

getClockStateOffset :: ClockState -> Int
getClockStateOffset ClockState { asList = a, timeOfDay = b, offset = c } = c


drawClockState :: State ClockState String 
drawClockState =
  get >>= \cs ->
    return $ drawClock (getClockStateTime cs)                           -- use getter i.e. TimeOfDay from ClockState
                                                                       

stringToInt :: String -> Int
stringToInt x = if all (`elem` "0123456789") x
                then read x :: Int
                else 0

-- Initialize ClockState to 00:00:00, current localtime and no offset
initClock :: IO ClockState
initClock = do
  now <- getCurrentTime                       -- eg 2022-07-13 17:25:29.4547484 ->UTC
  timezone <- getCurrentTimeZone              -- eg AEST
  let zoneNow = utcToLocalTime timezone now   -- eg 2022-07-14 03:24:41.7410000 ->Melb
  let timeOfDay = formatTime defaultTimeLocale "%l:%M:%S" zoneNow 
  let [h, m, s] = splitOn ":" timeOfDay
  let hours   = stringToInt h
  let minutes = stringToInt m
  let seconds = stringToInt s

  return $ ClockState {
     asList    = concat $ join [[bigNum 0], [spacer] ,[bigNum 0], [colonOn], [bigNum 0], [spacer], [bigNum 0], [colonOn], [bigNum 0], [spacer], [bigNum 0], [eol]]
    ,timeOfDay = (hours, minutes, seconds)
    ,offset    = 0
  }

utcToUtc :: UTCTime -> UTCTime
utcToUtc = addUTCTime (realToFrac 0)          -- get the offset from ClockState

-------------------------------------------------------------------
-- THREADS and EVENTS 
-- See final class notes on 05.07.22 

data Event = SecondsEvent | KeyEvent Char deriving Show 

ticker :: Chan Event -> IO () 
ticker chan = forever $ do 
  threadDelay (10 ^ 6)                                                          -- every 1 second is 10^6 microseconds
  writeChan chan SecondsEvent

input :: Chan Event -> IO () 
input chan = forever $ do  
  hSetEcho stdin False
  c <- getChar 
  hSetEcho stdin True
  writeChan chan (KeyEvent c)


-------------------------------------------------------------------
-- i.e. No ENTER required
noBuffering :: IO ()
noBuffering = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering


-------------------------------------------------------------------
-- MAIN ROUTINE 
main :: IO ()
main = do 
  noBuffering
  clearScreen
  hideCursor                                        
  hSetEcho stdin False                                                -- ensure the character isn't echoed back to the terminal. 
  setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Blue]
  
  -- heading animation
  forM_ [1..10] (\ i -> do                                            
    setCursorPosition 0 0 
    threadDelay (10 ^ 5)
    putStrLn $ concat $ drawVAnim i title
    )

  clockState <- initClock                                             -- set the initial clock state

                                                                      -- putStrLn $ drawClock $ getClockStateTime clockState
  setCursorPosition 10 0 
  setSGR [ SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Green] 
  putStrLn $ evalState drawClockState clockState                      -- Note evalState :: State s a -> s -> a
        

  setCursorPosition 20 0
  putStrLn "===== INSTRUCTIONS ====="
  putStrLn "Press 'x' to quit"
  putStrLn "Press '1' to '3' to change to color"
  putStrLn "Press 'e' to change TimeZone to AEST - Melbourne, Sydney, Brisbane"
  putStrLn "Press 'w' to change TimeZone to AWST - Perth"
  putStrLn "Press 'c' to change TimeZone to ACST - Adelaide, Darwin"


  chan <- newChan                                                     -- Channels and Events - see last class with fork.hs
  forkIO $ ticker chan 
  forkIO $ input chan
  
  forever $ do 
    c <- readChan chan
    case c of 
      
      -------- TICKER --------
      SecondsEvent  -> do

          now <- getCurrentTime                       -- eg 2022-07-13 17:25:29.4547484 ->UTC
          let now' = utcToUtc now
          timezone <- getCurrentTimeZone              -- eg AEST
          let zoneNow = utcToLocalTime timezone now'   -- eg 2022-07-14 03:24:41.7410000 ->Melb
          let timeOfDay = formatTime defaultTimeLocale "%l:%M:%S" zoneNow 
          let [h, m, s] = splitOn ":" timeOfDay
          let hours   = stringToInt h
          let minutes = stringToInt m
          let seconds = stringToInt s
          let offset  = 0

          let newClockState = ClockState { asList = [], timeOfDay = (hours, minutes, seconds), offset = offset }
          setCursorPosition 10 0
          putStrLn $ evalState drawClockState newClockState 


      -------- KEYEVENT --------
      KeyEvent c -> case c of  
        '1' -> do 
                setSGR [SetColor Foreground Vivid Green]
        '2' -> do 
                setSGR [SetColor Foreground Vivid Red]
        '3' -> do 
                setSGR [SetColor Foreground Vivid Yellow]
        'x' -> do                                           -- Exit Program
                putStrLn "Exit Clock"
                showCursor
                exitFailure
        'v' -> do                                           -- Dump the test data to screen
                setCursorPosition 25 0
                now <- getCurrentTime                       -- eg 2022-07-13 17:25:29.4547484 ->UTC
                timezone <- getCurrentTimeZone              -- eg AEST
                let zoneNow = utcToLocalTime timezone now   -- eg 2022-07-14 03:24:41.7410000 ->Melb
                print now

        -- TODO: Implement offsets
        -- 'e' -> do                                        -- Local server time
        --     let offset = 0
        --     put ClockState{offset = offset}
            
        -- 'c' -> do
        --     let offset = (-1800)
        --     put ClockState{offset = offset}

        -- 'w' -> do
        --     let offset = (-7200)
        --     put ClockState{offset = offset}

        _ -> putStr "" --putStr("Key : " ++ show [c])