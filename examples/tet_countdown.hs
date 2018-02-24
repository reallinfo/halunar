import Halunar
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

main = do
  now <- getCurrentTime
  timeZone <- getCurrentTimeZone
  let zoneNow = utcToLocalTime timeZone now
  let (year, month, day) = toGregorian $ localDay zoneNow
  let timeZoneInNumber = floor (fromIntegral (timeZoneMinutes timeZone) / 60)
  let (lDay, lMonth, lYear) =
        calculateLunarDateFromDate (day, month, fromIntegral year, timeZoneInNumber)
  let nextYear = lYear + 1
  let (tDay, tMonth, tYear) = calculateDateFromLunarDate (1, 1, nextYear, timeZoneInNumber)
  let currentDate = fromGregorian year month day
  let tetDate = fromGregorian (fromIntegral tYear) tMonth tDay
  let output = show (diffDays tetDate currentDate) ++ " days to Tet"
  putStrLn output
