import Control.Exception (assert)
import Data.Char (isDigit)

data Date = Date { day :: Int
                 , month :: Int
                 , year :: Int
                 }

fromRaw :: Int -> Int -> Int -> Date
fromRaw day' month' year' =
    let
        year = assert (year' > 0) year'
        month = assert (month' >= 1 && month' <= 12) month'
        day = let maxDayCount = _daysInMonthForYear month year in assert (day' >= 1 && day' <= maxDayCount) day'
    in
        Date { day = day, month = month, year = year }

_isLeapYear year = mod year 400 == 0 || (mod year 100 /= 0 && mod year 4 == 0)

_daysInMonthForYear :: Int -> Int -> Int
_daysInMonthForYear month year = case (month, _isLeapYear year) of
    (1, _)      ->  31
    (2, True)   ->  29
    (2, False)  ->  28
    (3, _)      ->  31
    (4, _)      ->  30
    (5, _)      ->  31
    (6, _)      ->  30
    (7, _)      ->  31
    (8, _)      ->  31
    (9, _)      ->  30
    (10, _)     ->  31
    (11, _)     ->  30
    (12, _)     ->  31

_daysInMonth Date { month = month', year = year' } = _daysInMonthForYear month' year'

instance Show Date where
    show :: Date -> String
    show Date { day = day
              , month = month
              , year = year
              }   = let
                        dayString = if day < 10 then "0" ++ show day else show day
                        monthString = if month < 10 then "0" ++ show month else show month
                    in
                        concat [dayString, ".", monthString, ".", show year]

instance Read Date where
    readsPrec :: Int -> ReadS Date
    readsPrec _ input =
        let
            (dayString, _ : noDayString) = break ((==) '.') input 
            (monthString, _ : noDayMonthString) = break ((==) '.') noDayString
            (yearString, other) = span isDigit noDayMonthString
        in
            [(fromRaw (read dayString) (read monthString) (read yearString), other)]

instance Eq Date where
    (==) :: Date -> Date -> Bool
    (==) first second = (day first == day second) && (month first == month second) && (year first == year second)

instance Ord Date where
    compare :: Date -> Date -> Ordering
    compare first second | year first /= year second    = compare (year first) (year second)
                         | month first /= month second  = compare (month first) (month second)
                         | day first /= day second      = compare (day first) (day second)
                         | otherwise                    = EQ

yearsBetween :: Date -> Date -> Int
yearsBetween first second = case compare first second of
    EQ  ->  0
    LT  ->  transform $ _yearsBetweenH first second 0
            where transform = max 0 . (flip (-)) (sameYearModifier first second)
                  sameYearModifier first second | month second < month first    = 1
                                                | month second > month first    = 0
                                                | day second < day first        = 1
                                                | otherwise                     = 0
    GT  ->  transform $ _yearsBetweenH second first 0
            where transform = min 0 . (+) (sameYearModifier first second) . negate
                  sameYearModifier first second | month first < month second    = 1
                                                | month first > month second    = 0
                                                | day first < day second        = 1
                                                | otherwise                     = 0

_yearsBetweenH :: Date -> Date -> Int -> Int
_yearsBetweenH first second count = if (year first) == (year second)
    then count
    else
        let
            closerYear = (year second) - 1
            month' = month second
            day' = min (_daysInMonthForYear month' closerYear) (day second)
        in
            _yearsBetweenH first Date { year = closerYear, month = month', day = day' } (count + 1)

monthsBetween :: Date -> Date -> Int
monthsBetween first second = case compare first second of
    EQ  ->  0
    LT  ->  transform $ _monthsBetweenH first second 0
            where transform = max 0 . (flip (-)) (sameMonthModifier first second)
                  sameMonthModifier first second = if day second < day first then 1 else 0
    GT  ->  transform $ _monthsBetweenH second first 0
            where transform = min 0 . (+) (sameMonthModifier first second) . negate
                  sameMonthModifier first second = if day first < day second then 1 else 0

_monthsBetweenH first second count =
    if year first == year second && month first == month second then count else _monthsBetweenH first second' (count + 1)
        where second' = if month second == 1
                then let year' = (year second) - 1
                         month' = 12
                         day' = min (day second) (_daysInMonthForYear month' year')
                     in Date { year = year', month = month', day = day' }
                else let year' = year second
                         month' = (month second) - 1
                         day' = min (day second) (_daysInMonthForYear month' year')
                     in Date { year = year', month = month', day = day' }

daysBetween :: Date -> Date -> Int
daysBetween first second = case compare first second of
    EQ  ->  0
    LT  ->  _daysBetweenH first second 0
    GT  ->  negate $ _daysBetweenH second first 0

_daysBetweenH first Date { year = year', month = month', day = day' } count
    | year first == year' &&
      month first == month'     = count + (day' - day first)
    | day' /= 1                 = _daysBetweenH first
                                                Date { year = year', month = month', day = 1 }
                                                count + day' - 1
    | month' == 1               = _daysBetweenH first
                                                Date { year = year' - 1, month = 12, day = 1 }
                                                count + 31
    | otherwise                 = _daysBetweenH first
                                                Date { year = year', month = month' - 1, day = 1 }
                                                count + _daysInMonthForYear (month' - 1) (year')

addYears :: Int -> Date -> Date
addYears years Date { year = year', month = month', day = day' }
    | month' == 2 && day' == 29 = let newYear = year' + years in
                                    if _isLeapYear newYear then Date { year = newYear, month = 02, day = 29 }
                                    else Date { year = newYear, month = 03, day = 01 }
    | otherwise                 = Date { year = year' + years, month = month', day = day' }

addMonths :: Int -> Date -> Date
addMonths months date@Date { year = year', month = month', day = day' }
    | months == 0                   = if day' > _daysInMonthForYear month' year'
                                        then Date { year = year', month = month' + 1, day = 1 }
                                        else date
    | months > 12 || months < -11   = let (d, m) = divMod months 12
                                      in addMonths m Date { year = year' + d, month = month', day = day' }
    | month' + months > 12          = addMonths 0 Date { year = year' + 1, month = month' + months - 12, day = day' }
    | month' + months < 1           = addMonths 0 Date { year = year' - 1, month = month' + months + 12, day = day' }
    | otherwise                     = addMonths 0 Date { year = year', month = month' + months, day = day' }

addDays :: Int -> Date -> Date
addDays days date@Date { year = year', month = month', day = day' }
    | day' + days >= 1 && day' + days <= _daysInMonth date = Date { year = year', month = month', day = day' + days }
    | day' /= 1                         = addDays (days + day' - 1) Date { year = year', month = month', day = 1 }
    | day' + days > _daysInMonth date   = if month' == 12
                                          then addDays (days - 31)
                                                       Date { year = year' + 1, month = 1, day = 1 }
                                          else addDays (days - _daysInMonth date)
                                                       Date { year = year', month = month' + 1, day = 1 }
    | day' + days < 1                   = if month' == 1
                                          then addDays (days + 31)
                                                       Date { year = year' - 1, month = 12, day = 1 }
                                          else addDays (days + _daysInMonthForYear (month' - 1) year')
                                                       Date { year = year', month = month' - 1, day = 1 }
