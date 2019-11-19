{-# LANGUAGE MultiWayIf #-}
import ParseLib.Abstract
import System.Environment

-- Starting Framework

-- | "Target" datatype for the DateTime parser, i.e, the parser should produce elements of this type.
data DateTime = DateTime { date :: Date
                         , time :: Time
                         , utc :: Bool }
    deriving (Eq, Ord)

data Date = Date { year  :: Year
                 , month :: Month
                 , day   :: Day }
    deriving (Eq, Ord)

newtype Year  = Year  { unYear :: Int }  deriving (Eq, Ord)
newtype Month = Month { unMonth :: Int } deriving (Eq, Ord)
newtype Day   = Day   { unDay :: Int } deriving (Eq, Ord)

data Time = Time { hour   :: Hour
                 , minute :: Minute
                 , second :: Second }
    deriving (Eq, Ord)

newtype Hour   = Hour { unHour :: Int } deriving (Eq, Ord)
newtype Minute = Minute { unMinute :: Int } deriving (Eq, Ord)
newtype Second = Second { unSecond :: Int } deriving (Eq, Ord)


-- | The main interaction function. Used for IO, do not edit.
data Result = SyntaxError | Invalid DateTime | Valid DateTime deriving (Eq, Ord)

instance Show DateTime where
    show = printDateTime

instance Show Result where
    show SyntaxError = "date/time with wrong syntax"
    show (Invalid _) = "good syntax, but invalid date or time values"
    show (Valid x)   = "valid date: " ++ show x

main :: IO ()
main = mainDateTime

mainDateTime :: IO ()
mainDateTime = interact (printOutput . processCheck . processInput)
    where
        processInput = map (run parseDateTime) . lines
        processCheck = map (maybe SyntaxError (\x -> if checkDateTime x then Valid x else Invalid x))
        printOutput  = unlines . map show

mainCalendar :: IO ()
mainCalendar = do
    file:_ <- getArgs
    res <- readCalendar file
    putStrLn $ maybe "Calendar parsing error" (ppMonth (Year 2012) (Month 11)) res

-- Exercise 1
parseDateTime :: Parser Char DateTime
parseDateTime = DateTime <$> 
    parseDate                   <*> 
    (symbol 'T' *> parseTime)   <*> 
    option (True <$ symbol 'Z') False

parseDate :: Parser Char Date
parseDate =  Date <$>
    (read <$> sequence (repeat 4 idDigit))  <*>
    (read <$> sequence (repeat 2 idDigit))  <*>
    (read <$> sequence (repeat 2 idDigit))

parseTime :: Parser Char Time
parseTime = Time <$>
    (read <$> sequence (repeat 2 idDigit)) <*>
    (read <$> sequence (repeat 2 idDigit))


-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p as = listToMaybe $ fst <$> (as >>= parse p)

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime (Date d) (Time t) u) = printDate d ++ "T" ++ printTime t ++ printkutc u

printDate :: Date -> String
printDate (Date (Year y) (Month m) (Day d)) = 
    let
    yb = if | y < 10    -> "000"
            | y < 100   -> "00" 
            | y < 1000  -> "0"
            | otherwise -> ""
    mb = if | m < 10    -> "0"
            | otherwise -> ""
    db = if | d < 10    -> "0"
            | otherwise -> ""
    in  yb ++ show y ++ mb ++ show m ++ db ++ show d

printDateTime :: Time -> String
printTime (Time (Hour h) (Minute m) (Second s)) = 
    let 
    hb = if | h < 10    -> "0"
            | otherwise -> ""
    mb = if | m < 10    -> "0"
            | otherwise -> ""
    sb = if | s < 10    -> "0"
            | otherwise -> ""    
    in hb ++ show h ++ mb ++ show m ++ sb ++ show s

printDateTime :: Bool -> String
printutc Fale = ""
printutc True = "Z"

-- Exercise 4
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5
checkDateTime :: DateTime -> Bool
checkDateTime (DateTime (Date d) (Time t) _) = checkDate d && checkTime t

checkDate :: Date -> Bool
checkDate (Date (Year y) (Month m) (Day d))
    | d < 1  = False
    | m > 12 = False
    | m `elem` [1, 3, 5, 7, 8, 10, 12] = d <= 31
    | m `elem` [4, 6, 9, 11] = d <= 30
    | m == 2 = if leapYear then d <= 29 else d <= 28 where
        leapYear = y `mod` 4 == 0 && y `mod` 100 /= 0 || y `mod` 400 == 0

checkTime :: Time -> Bool
checkTime (Time (Hour h) (Minute m) (Second s)) = h >= 0 && h <= 23 && m >= 0 && m <= 59 && s >= 0 && s <= 59

-- Exercise 6
data Calendar = Calendar{ prodId :: String
                        , calEvents:: [Event] }
    deriving (Eq, Ord, Show)
   

data Event = Event{ uId         :: String
                  , dtStamp     :: DateTime 
                  , dtStart     :: DateTime
                  , dtEnd       :: DateTime
                  , description :: Maybe String
                  , summary     :: Maybe String
                  , location    :: Maybe String }
    deriving (Eq, Ord, Show)
    

-- Exercise 7
data Token = PRODID String
           | VERSION
           | UID String
           | DTSTAMP DateTime
           | DTSTART DateTime
           | DTEND DateTime
           | Description String
           | Summary String
           | Location String
    deriving (Eq, Ord, Show)

scanCalendar :: Parser Char [Token]
scanCalendar = concat <$> sequence [scanHeader, scanEvent]

scanHeader :: Parser Char [Token]
scanHeader = (token "BEGIN:VCALENDAR") scanHeader' (token "END:VCALENDAR") where
    scanHeader' = many $ choice 
        [ PRODID  <$> pack (token "PRODID:") identifier (symbol  '\n')
        , VERSION <$  token "VERSION:2.0\n"]
    
scanEvent :: Parser Char [Token]
scanEvent = pack (token "BEGIN:VEVENT") scanEvent' (token "END:VEVENT") where 
    scanEvent' = many $ choice 
        [ DTSTAMP     <$> pack (token "DTSTAMP:"    ) parseDateTime (symbol  '\n')
        , DTSTART     <$> pack (token "DTSTART:"    ) parseDateTime (symbol  '\n')
        , DTEND       <$> pack (token "DTEND:"      ) parseDateTime (symbol  '\n')
        , UID         <$> pack (token "UID:"        ) identifier    (symbol  '\n')                 
        , Description <$> pack (token "DESCRIPTION:") identifier    (symbol  '\n')
        , Summary     <$> pack (token "SUMMARY:"    ) identifier    (symbol  '\n')
        , Location    <$> pack (token "LOCATION:"   ) identifier    (symbol  '\n')
        ]


parseCalendar :: Parser Token Calendar
parseCalendar = (sort <$> look) >>= parseCalendar' where
    parseCalendar' = Calendar <$> 

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8
readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar = undefined

-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar = undefined

-- Exercise 10
countEvents :: Calendar -> Int
countEvents = undefined

findEvents :: DateTime -> Calendar -> [Event]
findEvents = undefined

checkOverlapping :: Calendar -> Bool
checkOverlapping = undefined

timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

