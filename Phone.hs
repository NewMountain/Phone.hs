module Phone where

import Data.List
import Data.Char

data Buttons =
  Buttons { key :: Char
         , options :: [Char]
         }
  deriving (Eq, Ord, Show)

data Phone =
  Phone [Buttons]
  deriving (Eq, Ord, Show)

btn1  = Buttons '1' "1"
btn2  = Buttons '2' "abc2"
btn3  = Buttons '3' "def3"
btn4  = Buttons '4' "ghi4"
btn5  = Buttons '5' "jkl5"
btn6  = Buttons '6' "mno6"
btn7  = Buttons '7' "pqrs7"
btn8  = Buttons '8' "tuv8"
btn9  = Buttons '9' "wxyz9"
btn10 = Buttons '0' "+ _0"
btn11 = Buttons '#' ".,"

myPhone = Phone [ btn1
                , btn2
                , btn3
                , btn4
                , btn5
                , btn6
                , btn7
                , btn8
                , btn9
                , btn10
                , btn11
                ]

type Digit = Char

type Presses = Int

type ButtonAction = (Digit, Presses)

-- Helper function to take the value out of the maybe
maybePlucker :: Maybe a -> a
maybePlucker (Just a) = a

-- Helper function to get the index of a char out of a list of char
-- and +1 it for human consumption
getCharIndex :: Char -> String -> Int
getCharIndex s ls =
  ( (+1) $ maybePlucker $ findIndex (\x -> x == s) $ ls)


-- translate a charaction into its cell phone button clicks
-- remember what it was like being a teen in love on your RAZR?
-- Special handling as capitalization requires another ButtonAction
buttonActionMaker :: Buttons -> Char -> [ButtonAction]
buttonActionMaker btn s
  | elem s $ ['a'..'z'] ++ ['0'..'9'] ++  "+ _.," =
    [ mkBtn btn s ]
  | elem s ['A'..'Z'] =
    [ ('*', 1)
    , mkBtn btn ( toLower s )
    ]
  where
    mkBtn b st =
      ( (key b), ( getCharIndex st (options b) ) )

-- function to test if the char occurs in that buttons options
keypadSeeker :: Char -> Buttons -> [ButtonAction]
keypadSeeker s btn =
  case (elem (toLower s) $ options btn) of
    True -> buttonActionMaker btn s
    False -> [ ('x', 0) ]

-- map the char over all buttons and flatten and filter for success
reverseTaps :: Phone -> Char -> [ButtonAction]
reverseTaps (Phone btnList) s =
  filter (\(x, _) -> x /= 'x') $ concat $ map (keypadSeeker s) btnList


convoBreakDowner :: Phone -> [String] -> [[[ButtonAction]]]
convoBreakDowner phone noise =
  map (map (reverseTaps phone)) noise

convo :: [String]
convo =
  [ "Wanna play 20 questions",
    "Ya",
    "U 1st haha",
    "Lol ok. Have u ever tasted alcohol lol",
    "Lol ya",
    "Wow ur cool haha. Ur turn",
    "Ok. Do u think I am pretty Lol",
    "Lol ya",
    "Haha thanks just making sure rofl ur turn"
  ]
