{-# LANGUAGE MultiWayIf, LambdaCase, TypeApplications #-}
module Main where
    
import Prelude hiding ((*>), (<$), (<*))
import Data.List (sort)
import Data.Maybe(listToMaybe)
import Control.Monad(replicateM)
import qualified Data.Map as M
import Data.List.Split(chunksOf)
import Text.PrettyPrint.Boxes as B
import ParseLib.Abstract
import System.Environment
import Data.Char
import System.IO
import Data.Time hiding (Day,utc,parseTime)
import Data.Fixed

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
printTime (Time (Hour h) (Minute m) (Second s)) =  printT2 h ++ printT2 m ++ printT2 s

printT2 :: (Ord a, Num a, Show a) => a -> String
printT2 s = 
    if s < 10 
    then '0' : show s
    else show s
    
printutc :: Bool -> String
printutc False = ""
printutc True = "Z"

-- Exercise 4
parsePrint :: String -> Maybe String
parsePrint s = printDateTime <$> run parseDateTime s

-- Exercise 5

parseCheck :: String -> Maybe Bool
parseCheck s = checkDateTime <$> run parseDateTime s

checkDateTime :: DateTime -> Bool
checkDateTime (DateTime d t _) = checkDate d && checkTime t

checkDate :: Date -> Bool
checkDate (Date (Year y) (Month m) (Day d))
    | d < 1  = False
    | m < 1 || m > 12   = False
    | y < 0 || y > 9999 = False
    | otherwise         = d <= daysIn (Year y) (Month m)


daysIn :: Year -> Month -> Int 
daysIn y (Month m ) 
    | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
    | m `elem` [4, 6, 9, 11] = 30
    | m == 2 = if leapYear y then 29 else 28 
    | otherwise = 31
    
leapYear :: Year -> Bool
leapYear (Year y) = y `mod` 4 == 0 && y `mod` 100 /= 0 || y `mod` 400 == 0

checkTime :: Time -> Bool
checkTime (Time (Hour h) (Minute m) (Second s)) = h >= 0 && h <= 23 && m >= 0 && m <= 59 && s >= 0 && s <= 59

-- Exercise 6
data Calendar = Calendar{ prodId   :: String
                        , calEvents:: [Event]
                        , cKeyVal  :: [(String,String)] 
                        } deriving (Eq, Ord, Show)
   

data Event = Event{ uId         :: String
                  , dtStamp     :: DateTime 
                  , dtStart     :: DateTime
                  , dtEnd       :: DateTime
                  , description :: Maybe String
                  , summary     :: Maybe String
                  , location    :: Maybe String
                  , eKeyVal     :: [(String,String)]
                  } deriving (Eq, Ord, Show)
    

-- Exercise 7
data Token = PRODID String
           | VERSION
           | VEVENT [Token]
           | UID String
           | DTSTAMP DateTime
           | DTSTART DateTime
           | DTEND DateTime
           | DESCRIPTION String
           | SUMMARY String
           | LOCATION String
           | KEYVAL String String
           | JUNK
    deriving (Eq, Ord, Show)

calIdentifier' :: Parser Char String
calIdentifier' = some $ satisfy (/= '\r')

calIdentifier'' :: Parser Char String
calIdentifier'' = f <$> satisfy (== '\r') <*> satisfy (== '\n') <*> satisfy (== ' ')
                where f c c2 c3= [c,c2,c3] 

calIdentifier :: Parser Char String
calIdentifier = concat <$> listOf calIdentifier' calIdentifier''

calIdentifier2 :: Parser Char [String]
calIdentifier2 = listOf calIdentifier' calIdentifier''

isAlphaUpper :: Char -> Bool
isAlphaUpper x = isAlpha x && isUpper x

-- | Parses a specific given sequence of symbols.

notToken :: String -> Parser Char String
notToken []     = failp
notToken (x:xs) = ((:) <$> satisfy (==x) <*> notToken xs) <|> ((:) <$> satisfy (/=x) <*> calIdentifier)
  
--ugly solution for readingclub
notTokens :: String -> String -> Parser Char String
notTokens [] _    = failp
notTokens _ []    = failp
notTokens (x:xs) (y:ys)=  (:) <$> satisfy (\ c -> (c == x) || (c == y)) <*> notTokens xs ys <|> (:) <$> satisfy (\c-> (c/=x) && (c/=y)) <*> calIdentifier

--ugly solution for readingclub
skipLine :: Parser Char Token
skipLine = JUNK <$ notTokens "END:VCALENDAR" "END:VEVENT" <* calIdentifier <* token "\r\n"

readingClubTimezone :: Parser Char Token
readingClubTimezone = JUNK <$  greedy (satisfy (/=':'))

scanCalendar :: Parser Char [Token]
scanCalendar = pack (token "BEGIN:VCALENDAR\r\n") (greedy1 scanCalendar') (token "END:VCALENDAR\r\n") where 
    scanCalendar' = choice 
        [ PRODID   <$> pack (token "PRODID:") calIdentifier (token "\r\n")
        , VERSION  <$  token "VERSION:2.0\r\n"        
        , VEVENT   <$> pack (token "BEGIN:VEVENT\r\n") scanEvent (token "END:VEVENT\r\n")
        ]
        <<|> skipLine
    scanEvent = greedy1 $ choice 
        [ DTSTAMP     <$>  pack (token "DTSTAMP:" ) parseDateTime (token  "\r\n")
        , DTSTART     <$>  pack (token "DTSTART:" <|> token "DTSTART;" <* readingClubTimezone <* token ":") parseDateTime (token  "\r\n")
        , DTEND       <$>  pack (token "DTEND:"   <|> token "DTEND;"   <* readingClubTimezone <* token ":") parseDateTime (token  "\r\n")
        , UID         <$>  pack (token "UID:"        ) calIdentifier (token  "\r\n")                 
        , DESCRIPTION <$>  pack (token "DESCRIPTION:") calIdentifier (token  "\r\n")
        , SUMMARY     <$>  pack (token "SUMMARY:"    ) calIdentifier (token  "\r\n")
        , LOCATION    <$>  pack (token "LOCATION:"   ) calIdentifier  (token  "\r\n")
        ]
        <<|> skipLine

        
notSymbol :: Eq a => a -> Parser a a
notSymbol p = satisfy (/=p)
notSymbols :: Eq a => [a] -> Parser a a
notSymbols ps = satisfy (`notElem` ps)


-- | WIP: Not finished and not functional
kvS :: Parser Char Token
kvS = KEYVAL <$> greedy1 (notSymbols ":\r\n") <* symbol ':' <*> greedy1 (notSymbols ":\r\n") <* token "\r\n"

parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$> parseHeader <*> parseEvents <*> kvP


parseHeader :: Parser Token String
parseHeader = 
    ((\(PRODID s) -> s) <$> satisfy (\case (PRODID _) -> True ; _ -> False) <*  satisfy (\case VERSION -> True ; _ -> False) <|>
    (\(PRODID s) -> s) <$  satisfy (\case VERSION    -> True ; _ -> False) <*> satisfy (\case (PRODID _) -> True ; _ -> False))
    <* greedy (satisfy (\case JUNK -> True ; _ -> False))

parseEvents :: Parser Token [Event]
parseEvents = greedy $ anySymbol >>= parseEvent

parseEvent :: Token -> Parser Token Event
parseEvent (VEVENT es) = choice $ pure <$> event where
    event  = fst <$> parse parser sorted
    sorted = sort es
    parser = Event  <$> uidP
                    <*> dtstampP
                    <*> dtstartP
                    <*> dtendP
                    <*> descP
                    <*> summP
                    <*> locP
                    <*> kvP
                    <*  junkP 
                    <*  eof                      
parseEvent _ = failp @Token @Event

uidP :: Parser Token String
uidP     = fmap (\(UID n) -> n) (satisfy (\case (UID _)     -> True ; _    -> False))

dtstampP, dtstartP, dtendP :: Parser Token DateTime
dtstampP = fmap (\(DTSTAMP s) -> s) (satisfy (\case (DTSTAMP _) -> True ; _    -> False))
dtstartP = fmap (\(DTSTART s) -> s) (satisfy (\case (DTSTART _) -> True ; _    -> False))
dtendP   = fmap (\(DTEND s) -> s)   (satisfy (\case (DTEND _)   -> True ; _    -> False))

descP, summP, locP :: Parser Token (Maybe String)
descP    = fmap (fmap (\(DESCRIPTION s) -> s)) (optional $ satisfy (\case (DESCRIPTION _) -> True ; _ -> False))
summP    = fmap (fmap (\(SUMMARY s) -> s))     (optional $ satisfy (\case (SUMMARY _)     -> True ; _ -> False))
locP     = fmap (fmap (\(LOCATION s) -> s))    (optional $ satisfy (\case (LOCATION _)    -> True ; _ -> False))

kvP   :: Parser Token [(String,String)] 
kvP = greedy $ fmap (\(KEYVAL k v) -> (k,v)) (satisfy (\case (KEYVAL _ _ ) -> True ; _ -> False))

junkP :: Parser Token [Token]
junkP    = greedy (satisfy (\case JUNK -> True ; _ -> False)) 


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

parseTokensWith :: FilePath -> Parser Token b -> IO (Maybe b)
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
    foldr (\(k,v) acc -> acc ++ show k ++ show v ++ "\r\n") "" (cKeyVal c) ++ 
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
    "LOCATION:" ++ printMaybeS (location e)           ++ 
    foldr (\(k,v) acc -> acc ++ show k ++ show v ++ "\r\n") "" (eKeyVal e) ++
    "END:VEVENT\r\n"


printMaybeS :: Maybe String -> String
printMaybeS Nothing = ""
printMaybeS (Just s) = s ++ "\r\n"

-- Exercise 10
countEvents :: Calendar -> Int
countEvents (Calendar _ es _) = length es

findEvents :: DateTime -> Calendar -> [Event]
findEvents dt (Calendar _ es _) = filter (\e -> dtStart e <= dt && dtEnd e > dt) es

checkOverlapping :: Calendar -> Bool
checkOverlapping (Calendar _ es _) = not $ null [() | e1 <- es, e2 <- es, e1 /= e2 && overlap e1 e2] where
    overlap Event{dtStart = s1, dtEnd = e1} Event{dtStart = s2, dtEnd = e2} = e1 > s2 && e1 < e2 || e2 > s1 && e2 < e1

timeSpent :: String -> Calendar -> Int
timeSpent s (Calendar _ es _) =  sum (fmap timeSpent' (filter (\e -> summary e == Just s) es)) where
    timeSpent' Event{dtStart = start, dtEnd = end} = end <-> start
    e' <-> s' = fromInteger $ div' (nominalDiffTimeToSeconds $ diffUTCTime (dateTimeToUTC e') (dateTimeToUTC s')) 60 --div by 60 to convert to minutes

dateTimeToUTC :: DateTime -> UTCTime
dateTimeToUTC DateTime{date = d, time = t} = UTCTime { 
    utctDay = fromGregorian (toInteger $ unYear $ year d) (unMonth $ month d) (unDay $ day d), 
    utctDayTime = secondsToDiffTime $ toInteger $ unHour (hour t) *3600 + unMinute (minute t) * 60 + unSecond (second t)
    }
-- Exercise 11

-- import function that turns a date into a box
bxMonth :: Year -> Month -> Calendar -> Box
bxMonth yy mm (Calendar _ es _) = let
    maxDays     = daysIn yy mm 
    dBegin      = Date{ year = yy, month = mm, day = Day 1}
    dEnd        = Date{ year = yy, month = mm, day = Day maxDays }
    events      = filter (\Event{dtStart = s, dtEnd = e} -> date s >= dBegin && date e <= dEnd) es 
    -- | all the events that start the same day get added together in the map
    grouped     = M.fromListWith (flip (++)) $ map (\e -> (unDay $ day $ date $ dtStart e, [e])) events
    -- | the maximum amount of events in a day
    height       = foldr (\l cur -> max cur $ length l + 1) 1 grouped 
    -- | the width of a single elem in the calendar
    width       = 14 
    -- | empty days get added to the map                                                
    groupedPlus = foldr (\k -> M.insertWith var k []) grouped [1..maxDays]
    -- | turns h:m -> 0h:0m and keeps hh:mm as hh:mm
    simpTime t  = printT2 (unHour $ hour t) ++ ":" ++ printT2 (unMinute $ minute t) 
    -- | turns a list of events into a vertically stacked box of just the times
    toBox k l   = vcat left (text (show k) : map (\Event{ dtStart = s, dtEnd = e } -> text $ simpTime (time s) ++ " - " ++ simpTime (time e)) l)
    -- | each day as a box. Ordered by key so day 1 .. day 31
    dayBoxes    = map (align top left height width) $ M.elems $ M.mapWithKey toBox groupedPlus 
    vertline    = vcat left $ replicate height $ char '|'
    -- | splits the list into chunks of 7 (week length) and horizontally stitches those days together into a single box
    weekBoxes   = map (punctuateH top vertline) $ chunksOf 7 dayBoxes
    horLine     = hcat top $ replicate 6 (text $ replicate width '-' ++ "+") ++ [text $ replicate width '-']
    -- | vertically stitches the weeks together into a single box
    monthBox    = punctuateV left horLine weekBoxes
    in monthBox

-- opposite of const
var :: a -> b -> b
var _ b = b 

ppMonth :: Year -> Month -> Calendar -> String
ppMonth yy mm c = 
    let pretty = bxMonth yy mm c
    in render pretty
    

-- like ppMonth but prints the box to screen
printIOCalendar :: Year -> Month -> FilePath -> IO ()
printIOCalendar y m p = do 
        mc <- readCalendar p 
        case mc of 
            Nothing -> return ()
            Just c  -> 
                let pretty = ppMonth y m c
                in putStrLn pretty