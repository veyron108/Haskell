module MyChars (bigNum, starter, spacer, eol, colonOn, colonOff, title) where
    
    -------------------------------------------------------------------
-- NUMBER STRINGS
-- each bigNum is 10 col x 8 row characters
-- Notes: manytools.org/hacker-tools/ascii-banner for numbers

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

