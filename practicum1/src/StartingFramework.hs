{-# LANGUAGE MultiWayIf, LambdaCase #-}
module StartingFramework where
    

import Prelude hiding ((*>),sequence,(<$), (<*))
import Data.List (sort, find)
import Data.Functor (($>))
import Data.Maybe(listToMaybe, fromJust)
import Control.Monad(replicateM)
import ParseLib.Abstract
import System.Environment
import Debug.Trace
import System.IO

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
    (Year  . read <$> replicateM 4 digit)  <*>
    (Month . read <$> replicateM 2 digit) <*>
    (Day   . read <$> replicateM 2 digit)

parseTime :: Parser Char Time
parseTime = Time <$>
    (Hour   . read <$> replicateM 2 digit) <*>
    (Minute . read <$> replicateM 2 digit) <*>
    (Second . read <$> replicateM 2 digit)


-- Exercise 2
run :: Parser a b -> [a] -> Maybe b
run p as = listToMaybe $ map fst (parse p as)

-- Exercise 3
printDateTime :: DateTime -> String
printDateTime (DateTime d t u) = printDate d ++ "T" ++ printTime t ++ printutc u

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

printTime :: Time -> String
printTime (Time (Hour h) (Minute m) (Second s)) = 
    let 
    check n = if | n < 10    -> "0"
                 | otherwise -> ""
    in check h ++ show h ++ check m ++ show m ++ check s ++ show s

printutc :: Bool -> String
printutc False = ""
printutc True = "Z"

-- Exercise 4
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5

parseCheck s = checkDateTime <$> run parseDateTime s

checkDateTime :: DateTime -> Bool
checkDateTime (DateTime d t _) = checkDate d && checkTime t

checkDate :: Date -> Bool
checkDate (Date (Year y) (Month m) (Day d))
    | d < 1  = False
    | m < 1 || m > 12 = False
    | y < 0 ||  y > 9999 = False
    | m `elem` [1, 3, 5, 7, 8, 10, 12] = d <= 31
    | m `elem` [4, 6, 9, 11] = d <= 30
    | m == 2 = if leapYear then d <= 29 else d <= 28 where
        leapYear = y `mod` 4 == 0 && y `mod` 100 /= 0 || y `mod` 400 == 0

checkTime :: Time -> Bool
checkTime (Time (Hour h) (Minute m) (Second s)) = h >= 0 && h <= 23 && m >= 0 && m <= 59 && s >= 0 && s <= 59

-- Exercise 6
data Calendar = Calendar{ prodId   :: String
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
data Token = VCALENDAR [Token] [Token]
           | PRODID String
           | VERSION
           | VEVENT [Token]
           | UID String
           | DTSTAMP DateTime
           | DTSTART DateTime
           | DTEND DateTime
           | Description String
           | Summary String
           | Location String
    deriving (Eq, Ord, Show)

calIdentifier = greedy $ satisfy (\c -> c /= '\r' && c /= '\n')

scanCalendar :: Parser Char [Token]
scanCalendar = greedy $ VCALENDAR <$ token "BEGIN:VCALENDAR\r\n" <*> scanHeader <*> scanEvent <* token "END:VCALENDAR\r\n"

scanHeader :: Parser Char [Token]
scanHeader = greedy1 $ choice 
        [ PRODID  <$> pack (token "PRODID:") calIdentifier (token "\r\n")
        , VERSION <$  token "VERSION:2.0\r\n"]
    
scanEvent :: Parser Char [Token]
scanEvent = greedy1 $ VEVENT <$ token "BEGIN:VEVENT\r\n" <*> scanEvent' <* token "END:VEVENT\r\n" where 
    scanEvent' = greedy1 $ choice 
        [ DTSTAMP     <$>  pack (token "DTSTAMP:"    ) parseDateTime (token  "\r\n")
        , DTSTART     <$>  pack (token "DTSTART:"    ) parseDateTime (token  "\r\n")
        , DTEND       <$>  pack (token "DTEND:"      ) parseDateTime (token  "\r\n")
        , UID         <$>  pack (token "UID:"        ) calIdentifier (token  "\r\n")                 
        , Description <$>  pack (token "DESCRIPTION:") calIdentifier (token  "\r\n")
        , Summary     <$>  pack (token "SUMMARY:"    ) calIdentifier (token  "\r\n")
        , Location    <$>  pack (token "LOCATION:"   ) calIdentifier  (token  "\r\n")
        ]

parseCalendar :: Parser Token Calendar
parseCalendar =  head . parseCalendar' <$> satisfy (\case (VCALENDAR _ _) -> True ; _ -> False) where
    parseCalendar' (VCALENDAR h e) = Calendar <$> pId <*> events where 
        pId    = fst <$> parse parseHeader (sort h)
        events = fst <$> parse parseEvents (sort e)
    


parseHeader :: Parser Token String
parseHeader = (\(PRODID s) -> s) <$> satisfy (\case (PRODID _) -> True ; _ -> False) <*  satisfy (\case VERSION -> True ; _ -> False)

parseEvents :: Parser Token [Event]
parseEvents = greedy $ fromEvent <$> satisfy isEvent

isEvent :: Token -> Bool
isEvent (VEVENT _) = True
isEvent _          = False

fromEvent :: Token -> Event
fromEvent (VEVENT es) = Event { 
  uId         = uid
, dtStamp     = stamp 
, dtStart     = start
, dtEnd       = end
, description = des
, summary     = sum
, location    = loc }
    where uid  =  fromUID (fromJust (find isUID es))
          stamp = fromDTSTAMP (fromJust (find isDTSTAMP es))
          start = fromDTSTART (fromJust (find isDTSTART es))
          end   = fromDTEND (fromJust (find isDTEND es))
          des   = fromDescription (find isDescription es)
          sum   = fromSummary (find isSummary es)
          loc   = fromLocation (find isLocation es)

isUID :: Token -> Bool
isUID (UID _) = True
isUID _          = False

fromUID :: Token -> String
fromUID (UID uid) = uid
UID _          = error "fromUID"

isDTSTAMP :: Token -> Bool
isDTSTAMP (DTSTAMP _) = True
isDTSTAMP _          = False

fromDTSTAMP :: Token -> DateTime
fromDTSTAMP (DTSTAMP stamp) = stamp
DTSTAMP _          = error "fromDTSTAMP"

isDTSTART :: Token -> Bool
isDTSTART (DTSTART _) = True
isDTSTART _          = False

fromDTSTART :: Token -> DateTime
fromDTSTART (DTSTART st) = st
DTSTART _          = error "fromDTSTART"

isDTEND :: Token -> Bool
isDTEND (DTEND _) = True
isDTEND _          = False

fromDTEND :: Token -> DateTime
fromDTEND (DTEND end) = end
fromDTEND _          = error "fromDTEND"

isDescription :: Token -> Bool
isDescription (Description _) = True
isDescription _          = False

fromDescription :: Maybe Token -> Maybe String
fromDescription (Just (Description des)) = Just des
fromDescription _          = Nothing

isSummary:: Token -> Bool
isSummary (Summary _) = True
isSummary _          = False

fromSummary :: Maybe Token -> Maybe String
fromSummary (Just (Summary sum)) = Just sum
fromSummary _          = Nothing

isLocation :: Token -> Bool
isLocation (Location _) = True
isLocation _          = False

fromLocation :: Maybe Token -> Maybe String
fromLocation (Just (Location loc)) = Just loc
fromLocation _          = Nothing

recognizeCalendar :: String -> Maybe Calendar
recognizeCalendar s = run scanCalendar s >>= run parseCalendar

-- Exercise 8

-- for testing
parseStrWith :: FilePath -> Parser Char a -> IO (Maybe a)
parseStrWith p prs = do
    handle  <- openFile p ReadMode
    _       <- hSetNewlineMode handle noNewlineTranslation
    content <- hGetContents handle
    return $ run prs content

parseTokensWith p prs = do
    tokens <- readTokens p
    return $ tokens >>= run prs 

-- for testing tokenisation
readTokens :: FilePath -> IO (Maybe [Token])
readTokens p = parseStrWith p scanCalendar 

readCalendar :: FilePath -> IO (Maybe Calendar)
readCalendar p = parseTokensWith p parseCalendar


-- Exercise 9
-- DO NOT use a derived Show instance. Your printing style needs to be nicer than that :)
printCalendar :: Calendar -> String
printCalendar c = 
    "BEGIN:VCALENDAR\r\n"           ++
    "PRODID:" ++ prodId c ++ "\r\n" ++
    "VERSION:2.0\r\n"               ++
    (calEvents c >>= printEvent)    ++
    "END:VCALENDAR"

printEvent :: Event -> String
printEvent e = 
    "BEGIN:VEVENT\r\n"                                ++
    "UID:" ++ uId e ++ "\r\n"                         ++
    "DTSTAMP:" ++ printDateTime (dtStamp e) ++ "\r\n" ++
    "DTSTART:" ++ printDateTime (dtStart e) ++ "\r\n" ++
    "DTSEND:"  ++ printDateTime (dtEnd   e) ++ "\r\n" ++
    "DESCRIPTION:" ++ printMaybeS (description e)     ++
    "SUMMARY:"  ++ printMaybeS (summary e)            ++
    "LOCATION:" ++ printMaybeS (location e)    


printMaybeS :: Maybe String -> String
printMaybeS Nothing = ""
printMaybeS (Just s) = s ++ "\r\n"

-- Exercise 10
countEvents :: Calendar -> Int
countEvents (Calendar _ es) = length es

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar _ es) = filter (\e -> dtStart e <= dt && dtEnd e > dt) es

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ es) = not $ null [() | e1 <- es, e2 <- es, e1 /= e2 && overlap e1 e2] where
    overlap Event{dtStart = s1, dtEnd = e1} Event{dtStart = s2, dtEnd = e2} = e1 > s2 && e1 < e2 || e2 > s1 && e2 < e1

timeSpent :: String -> Calendar -> Int
timeSpent = undefined

-- Exercise 11
ppMonth :: Year -> Month -> Calendar -> String
ppMonth = undefined

