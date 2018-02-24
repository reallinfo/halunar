import Halunar
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime

main = do
  now <- getCurrentTime
  timeZone <- getCurrentTimeZone
  let zoneNow = utcToLocalTime timeZone now
  let (year, month, day) = toGregorian $ localDay zoneNow
  let (lDay, lMonth, lYear) =
        calculateLunarDateFromDate (day, month, fromIntegral year, floor (fromIntegral (timeZoneMinutes timeZone) / 60))
  let output = show lDay ++ "-" ++ show lMonth ++ "-" ++ show lYear
  putStrLn output
