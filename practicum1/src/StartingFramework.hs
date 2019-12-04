{-# LANGUAGE MultiWayIf, LambdaCase, TypeApplications #-}
module StartingFramework where
    

import Prelude hiding ((*>), sequence, (<$), (<*))
import Data.List (sort, groupBy, sortBy)
import Data.Maybe(listToMaybe)
import Control.Monad(replicateM, join, forM_)
import Data.Ord(comparing)
import qualified Data.Map as M
import Data.List.Split(chunksOf)
import Text.PrettyPrint.Boxes as B
import ParseLib.Abstract
import System.Environment
import Data.Char
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
    | m < 1 || m > 12   = False
    | y < 0 || y > 9999 = False
    | otherwise         = d <= daysIn (Year y) (Month m)


daysIn :: Year -> Month -> Int 
daysIn y (Month m ) 
    | m `elem` [1, 3, 5, 7, 8, 10, 12] = 31
    | m `elem` [4, 6, 9, 11] = 30
    | m == 2 = if leapYear y then 29 else 28 
    
leapYear :: Year -> Bool
leapYear (Year y) = y `mod` 4 == 0 && y `mod` 100 /= 0 || y `mod` 400 == 0

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
           | JUNK
    deriving (Eq, Ord, Show)

calIdentifier' = greedy $ satisfy (\c -> c /= '\r')

calIdentifier'' :: Parser Char String
calIdentifier'' = f <$> satisfy (== '\r') <*> satisfy (== '\n') <*> satisfy (== ' ')
                where f c c2 c3= [c,c2,c3] 

calIdentifier :: Parser Char String
calIdentifier = concat <$> listOf calIdentifier' calIdentifier''

calIdentifier2 :: Parser Char [String]
calIdentifier2 = listOf calIdentifier' calIdentifier''
isAlphaUpper x = isAlpha x && isUpper x
-- | Parses a specific given sequence of symbols.
notToken :: [Char] -> Parser Char String
notToken []     = failp
notToken (x:xs) = ((:)<$>satisfy (==x) <*> notToken xs) <|> ((:)<$>satisfy (/=x) <*> calIdentifier)
           
notTokens :: [Char] -> [Char] -> Parser Char String
notTokens [] _    = failp
notTokens _ []    = failp
notTokens (x:xs) (y:ys)= ((:)<$>satisfy (\c -> (c==x)||(c==y)) <*> (notTokens xs ys)) <|> ((:)<$>satisfy (\c-> (c/=x) && (c/=y)) <*> calIdentifier)

skipLine = JUNK  <$  ((notTokens "END:VCALENDAR" "END:VEVENT") ) <* calIdentifier <* (token "\r\n")
scanCalendar :: Parser Char [Token]
scanCalendar = pack (token "BEGIN:VCALENDAR\r\n") (greedy scanCalendar') (token "END:VCALENDAR\r\n") where 
    scanCalendar' = choice 
        [ PRODID   <$> pack (token "PRODID:") calIdentifier (token "\r\n")
        , VERSION  <$  token "VERSION:2.0\r\n"        
        , VEVENT   <$> pack (token "BEGIN:VEVENT\r\n") scanEvent (token "END:VEVENT\r\n")
        ] <<|>skipLine
    scanEvent = greedy1 $ choice 
        [ DTSTAMP     <$>  pack (token "DTSTAMP:"    ) parseDateTime (token  "\r\n")
        , DTSTART     <$>  pack (token "DTSTART:"    ) parseDateTime (token  "\r\n")
        , DTEND       <$>  pack (token "DTEND:"      ) parseDateTime (token  "\r\n")
        , UID         <$>  pack (token "UID:"        ) calIdentifier (token  "\r\n")                 
        , DESCRIPTION <$>  pack (token "DESCRIPTION:") calIdentifier (token  "\r\n")
        , SUMMARY     <$>  pack (token "SUMMARY:"    ) calIdentifier (token  "\r\n")
        , LOCATION    <$>  pack (token "LOCATION:"   ) calIdentifier  (token  "\r\n")
        ] <<|> skipLine

-- very difficult to figure this one out imo.
parseCalendar :: Parser Token Calendar
parseCalendar = Calendar <$> parseHeader <*> parseEvents


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
    parser = Event  <$> fmap (\(UID n) -> n)                (satisfy (\case (UID _)     -> True ; _    -> False))
                    <*> fmap (\(DTSTAMP s) -> s)            (satisfy (\case (DTSTAMP _) -> True ; _    -> False))
                    <*> fmap (\(DTSTART s) -> s)            (satisfy (\case (DTSTART _) -> True ; _    -> False))
                    <*> fmap (\(DTEND s) -> s)              (satisfy (\case (DTEND _)   -> True ; _    -> False))
                    <*> fmap (fmap (\(DESCRIPTION s) -> s)) (optional $ satisfy (\case (DESCRIPTION _) -> True ; _ -> False))
                    <*> fmap (fmap (\(SUMMARY s) -> s))     (optional $ satisfy (\case (SUMMARY _)     -> True ; _ -> False))
                    <*> fmap (fmap (\(LOCATION s) -> s))    (optional $ satisfy (\case (LOCATION _)    -> True ; _ -> False))
                    <*  eof
parseEvent _ = failp @Token @Event


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
timeSpent s (Calendar _ es)=  sum (fmap timeSpent' (filter (\e -> summary e == Just s) es)) where
    timeSpent' Event{dtStart = s1, dtEnd = e1} =s1 <-> e1
    (<->) DateTime { date = Date {day = Day {unDay = dd}}  , time = Time {hour = Hour {unHour = hh} , minute =Minute {unMinute =mm}  , second = Second {unSecond = ss}}}  
          DateTime { date = Date {day = Day {unDay = dd2}} , time = Time {hour = Hour {unHour = hh2}, minute = Minute {unMinute =mm2}, second = Second {unSecond =ss2}}} 
          = ((ss-ss2) `mod` 60) + mm-mm2 + 60 *(hh-hh2)+60 * 24 * (dd - dd2)
    

-- Exercise 11

bxMonth :: Year -> Month -> Calendar -> Box
bxMonth yy mm (Calendar _ es) = let
    maxDays = daysIn yy mm 
    dBegin = Date{ year = yy, month = mm, day = Day 1}
    dEnd = Date{ year = yy, month = mm, day = Day maxDays }
    events = filter (\Event{dtStamp = s, dtEnd = e} -> date s >= dBegin && date e <= dEnd) es 
    grouped =  M.fromListWith (++) $ map (\e -> (unDay $ day $ date $ dtStart e, [e])) events
    groupedPlus = foldr (\k -> M.insertWith keep k []) grouped [1..maxDays]
    toBox k l = vcat left (para left 10 (show k) : map (\ Event{} -> para left 10 "-bla-") l)
    dayBoxes = M.elems $ M.mapWithKey toBox groupedPlus 
    weekBoxes = map (hcat top) $ chunksOf 7 dayBoxes
    monthBox = vcat left weekBoxes
    in monthBox

keep a b = b 

ppMonth :: Year -> Month -> Calendar -> String
ppMonth yy mm c = 
    let pretty = bxMonth yy mm c
    in render pretty
    

printIOCalendar :: Year -> Month -> FilePath -> IO ()
printIOCalendar y m p = do 
        mc <- readCalendar p 
        case mc of 
            Nothing -> return ()
            Just c  -> 
                let pretty = ppMonth y m c
                in putStrLn pretty