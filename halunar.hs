module Halunar
  ( calculateDateFromLunarDate
  , calculateLunarDateFromDate
  ) where

calculateJuliusDayFromDate :: (Int, Int, Int) -> Int
calculateJuliusDayFromDate (day, month, year) =
  day + ((153 * m + 2) `div` 5) + 365 * y + (y `div` 4) - (y `div` 100) + (y `div` 400) - 32045
    where
      a = (14 - month) `div` 12
      m = month + 12 * a - 3
      y = year + 4800 - a

calculateFactorsFromJuliusDay :: Int -> (Int, Int, Int)
calculateFactorsFromJuliusDay juliusDay
  | juliusDay > 2299160 = (a, b, c)
  | otherwise = (-1, 0, d)
    where
      a = juliusDay + 32044
      b = (4 * a + 3) `div` 146097
      c = a - ((b * 146097) `div` 4)
      d = juliusDay + 32082

calculateDateFromFactors :: (Int, Int, Int) -> (Int, Int, Int)
calculateDateFromFactors (a, b, c) =
  (e - ((153 * m + 2) `div` 5) + 1, m + 3 - 12 * (m `div` 10), b * 100 + d - 4800 + (m `div` 10))
      where
        d = (4 * c + 3) `div` 1461
        e = c - ((1461 * d) `div` 4)
        m = (5 * e + 2) `div` 153

calculateDateFromJuliusDay :: Int -> (Int, Int, Int)
calculateDateFromJuliusDay juliusDay =
  calculateDateFromFactors (calculateFactorsFromJuliusDay juliusDay)

calculateT1 :: Int -> Float
calculateT1 numberOfMonths = fromIntegral numberOfMonths / 1236.85

calculateDeltaT :: Float -> Float
calculateDeltaT t1
  | t1 < -11 = 0.001 + 0.000839 * t1 + 0.0002261 * t2 - 0.00000845 * t3 - 0.000000081* t1* t3
  | otherwise = -0.000278 + 0.000265 * t1 + 0.000262 * t2
    where
      t2 = t1 ^ 2
      t3 = t1 ^ 3

calculateLatitude :: Int -> Float
calculateLatitude numberOfMonths =
  c + 0.0010 * sin (dr * (2 * f - mpr)) + 0.0005 * sin (dr * (2 * mpr + m))
    where
      k1 = fromIntegral numberOfMonths
      t1 = calculateT1 numberOfMonths
      t2 = t1 ^ 2
      t3 = t1 ^ 3
      dr = pi / 180
      f = 21.2964 + 390.67050646 * k1 - 0.0016528 * t2 - 0.00000239 * t3
      mpr = 306.0253 + 385.81691806 * k1 + 0.0107306 * t2 + 0.00001236 * t3
      m = 359.2242 + 29.10535608 * k1 - 0.0000333 * t2 - 0.00000347 * t3
      c5 = (0.1734 - 0.000393 * t1) * sin (m * dr) + 0.0021 * sin (2 * dr * m)
      c4 = c5 - 0.4068 * sin (mpr * dr) + 0.0161 * sin (dr * 2 * mpr)
      c3 = c4 - 0.0004 * sin (dr * 3 * mpr)
      c2 = c3 + 0.0104 * sin (dr * 2 * f) - 0.0051 * sin (dr * (m + mpr))
      c1 = c2 - 0.0074 * sin (dr * (m - mpr)) + 0.0004 * sin (dr * (2 * f + m))
      c = c1 - 0.0004 * sin (dr * (2 * f - m)) - 0.0006 * sin (dr * (2 * f + mpr))

calculateNewMoonDay :: Int -> Int -> Int
calculateNewMoonDay numberOfMonths timeZone =
  floor (jdNewMoon + 0.5 + fromIntegral timeZone / 24)
    where
      k1 = fromIntegral numberOfMonths
      dr = pi / 180
      t1 = calculateT1 numberOfMonths
      t2 = t1 ^ 2
      t3 = t1 ^3
      deltaT = calculateDeltaT t1
      c = calculateLatitude numberOfMonths
      jd2 = 2415020.75933 + 29.53058868 * k1 + 0.0001178 * t2 - 0.000000155 * t3
      jd1 = jd2 + 0.00033 * sin ((166.56 + 132.87 * t1 - 0.009173 * t2) * dr)
      jdNewMoon = jd1 + c - deltaT

calculateSunLongitude :: Int -> Int -> Int
calculateSunLongitude juliusDay timeZone =
  floor (l / pi * 6)
    where
      dr = pi / 180
      t1 = (fromIntegral juliusDay - 2451545.5 - fromIntegral timeZone / 24) / 36525
      t2 = t1 ^ 2
      m = 357.52910 + 35999.05030 * t1 - 0.0001559 * t2 - 0.00000048 * t1 * t2
      dl1 = (1.914600 - 0.004817 * t1 - 0.000014 * t2) * sin (dr * m)
      dl = dl1 + (0.019993 - 0.000101 * t1) * sin (dr * 2 * m) + 0.000290 * sin (dr * 3 * m)
      l3 = 280.46645 + 36000.76983 * t1 + 0.0003032 * t2
      l2 = l3 + dl
      l1 = l2 * dr
      l = l1 - pi * 2 * fromIntegral (floor (l1 / (pi * 2)))

calculateLunarMonthEleven :: Int -> Int -> Int
calculateLunarMonthEleven year timeZone
  | sunLng >= 9 = calculateNewMoonDay (numberOfMonths - 1) timeZone
  | otherwise = calculateNewMoonDay numberOfMonths timeZone
    where
      off = calculateJuliusDayFromDate (31, 12, year) - 2415021
      numberOfMonths = floor (fromIntegral off / 29.530588853)
      nm = calculateNewMoonDay numberOfMonths timeZone
      sunLng = calculateSunLongitude nm timeZone

calculateLeapMonthOffsetFromFactors :: Int -> Int -> Int -> Int -> Int -> Int
calculateLeapMonthOffsetFromFactors m11 timeZone last arc index
  | arc /= last && index < 14 =
    calculateLeapMonthOffsetFromFactors m11 timeZone tmpLast tmpArc tmpIndex
  | otherwise = index - 1
    where
      numberOfMonths =
        floor ((fromIntegral m11 - 2415021.076998695) / 29.530588853 + 0.5)
      tmpLast = last
      tmpArc =
        calculateSunLongitude (calculateNewMoonDay (numberOfMonths + index) timeZone) timeZone
      tmpIndex = tmpIndex + 1

calculateLeapMonthOffset :: Int -> Int -> Int
calculateLeapMonthOffset m11 timeZone =
  calculateLeapMonthOffsetFromFactors m11 timeZone last arc index where
    numberOfMonths =
      floor ((fromIntegral m11 - 2415021.076998695) / 29.530588853 + 0.5)
    arc =
      calculateSunLongitude (calculateNewMoonDay (numberOfMonths + 1)  timeZone) timeZone
    last = 0
    index = 1

calculateLunarMonthStartAt :: (Int, Int, Int, Int) -> Int
calculateLunarMonthStartAt (day, month, year, timeZone)
  | lMonthStartAt > dayNumber = calculateNewMoonDay numberOfMonths timeZone
  | otherwise = calculateNewMoonDay (numberOfMonths + 1) timeZone
    where
      dayNumber = calculateJuliusDayFromDate (day, month, year)
      numberOfMonths =
        floor ((fromIntegral dayNumber - 2415021.076998695) / 29.530588853)
      lMonthStartAt = calculateNewMoonDay (numberOfMonths + 1) timeZone

calculateLunarDayFromDate :: (Int, Int, Int, Int) -> Int
calculateLunarDayFromDate (day, month, year, timeZone) =
  dayNumber - lMonthStartAt + 1 where
    dayNumber = calculateJuliusDayFromDate (day, month, year)
    numberOfMonths =
      floor ((fromIntegral dayNumber - 2415021.076998695) / 29.530588853)
    lMonthStartAt = calculateLunarMonthStartAt (day, month, year, timeZone)

calculateLunarFactorsFromDate :: (Int, Int, Int, Int) -> (Int, Int)
calculateLunarFactorsFromDate (day, month, year, timeZone)
  | a11 >= lMonthStartAt = (calculateLunarMonthEleven (year - 1) timeZone, a11)
  | otherwise = (a11, calculateLunarMonthEleven (year + 1) timeZone)
    where
      lMonthStartAt = calculateLunarMonthStartAt (day, month, year, timeZone)
      a11 = calculateLunarMonthEleven year timeZone

calculateLunarMonthWithFactors :: (Int, Int, Int, Int) -> Int
calculateLunarMonthWithFactors (day, month, year, timeZone)
  | b11 - a11 > 365 && diff >= leapMonthDiff = diff + 10
  | otherwise = diff + 11
    where
      lMonthStartAt = calculateLunarMonthStartAt (day, month, year, timeZone)
      diff = floor (fromIntegral (lMonthStartAt - a11) / 29)
      leapMonthDiff = calculateLeapMonthOffset a11 timeZone
      (a11, b11) = calculateLunarFactorsFromDate (day, month, year, timeZone)

calculateLunarMonthFromDate :: (Int, Int, Int, Int) -> Int
calculateLunarMonthFromDate (day, month, year, timeZone)
  | lMonth > 12 = lMonth - 12
  | otherwise = lMonth
    where
      lMonth = calculateLunarMonthWithFactors (day, month, year, timeZone)

calculateLunarYearWithFactors :: (Int, Int, Int, Int) -> Int
calculateLunarYearWithFactors (day, month, year, timeZone)
  | a11 >= lMonthStartAt = year
  | otherwise = year + 1
    where
      lMonthStartAt = calculateLunarMonthStartAt (day, month, year, timeZone)
      a11 = calculateLunarMonthEleven year timeZone

calculateLunarYearFromDate :: (Int, Int, Int, Int) -> Int
calculateLunarYearFromDate (day, month, year, timeZone)
  | lMonth >= 11 && diff < 4 = lYear - 1
  | otherwise = lYear
    where
      (a11, b11) = calculateLunarFactorsFromDate (day, month, year, timeZone)
      diff = floor (fromIntegral (lMonthStartAt - a11) / 29)
      lMonth = calculateLunarMonthFromDate (day, month, year, timeZone)
      lMonthStartAt = calculateLunarMonthStartAt (day, month, year, timeZone)
      lYear = calculateLunarYearWithFactors (day, month, year, timeZone)

isLunarLeapYear :: Int -> Bool
isLunarLeapYear lYear = (lYear `mod` 19) `elem` [0, 3, 6, 9, 11, 14, 17]

calculateLunarDateFromDate :: (Int, Int, Int, Int) -> (Int, Int, Int)
calculateLunarDateFromDate (day, month, year, timeZone) =
  (lDay, lMonth, lYear) where
    lDay = calculateLunarDayFromDate (day, month, year, timeZone)
    lMonth = calculateLunarMonthFromDate (day, month, year, timeZone)
    lYear = calculateLunarYearFromDate (day, month, year, timeZone)

calculateLunarFactorsFromLunarDate :: (Int, Int, Int, Int) -> (Int, Int)
calculateLunarFactorsFromLunarDate (lDay, lMonth, lYear, timeZone)
  | lMonth < 11 =
    (calculateLunarMonthEleven (lYear - 1) timeZone, calculateLunarMonthEleven lYear timeZone)
  | otherwise =
    (calculateLunarMonthEleven lYear timeZone, calculateLunarMonthEleven (lYear + 1) timeZone)

calculateLunarSolarOffsetByMonth :: Int -> Int
calculateLunarSolarOffsetByMonth lMonth
  | offset < 0 = offset + 12
  | otherwise = offset
    where
      offset = lMonth - 11

calculateLunarSolarOffset :: (Int, Int, Int, Int) -> Int
calculateLunarSolarOffset (lDay, lMonth, lYear, timeZone)
  | b11 - a11 > 365 && (isLunarLeapYear lYear || offset >= leapOffset) =
    offset + 1
  | otherwise = offset
    where
      offset = calculateLunarSolarOffsetByMonth lMonth
      leapOffset = calculateLeapMonthOffset a11 timeZone
      (a11, b11) =
        calculateLunarFactorsFromLunarDate (lDay, lMonth, lYear, timeZone)

calculateDateFromLunarDate :: (Int, Int, Int, Int) -> (Int, Int, Int)
calculateDateFromLunarDate (lDay, lMonth, lYear, timeZone) =
  calculateDateFromJuliusDay (lMonthStartAt + lDay - 1)
    where
      (a11, b11) =
        calculateLunarFactorsFromLunarDate (lDay, lMonth, lYear, timeZone)
      numberOfMonths = floor (0.5 + (fromIntegral a11 - 2415021.076998695) / 29.530588853)
      offset = calculateLunarSolarOffset (lDay, lMonth, lYear, timeZone)
      lMonthStartAt = calculateNewMoonDay (numberOfMonths + offset) timeZone
