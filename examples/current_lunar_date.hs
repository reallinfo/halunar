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
  let output = show lDay ++ "-" ++ show lMonth ++ "-" ++ show lYear ++ " (dd/mm/yyyy)"
  putStrLn output
