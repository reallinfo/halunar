jdFromDate :: Int -> Int -> Int -> Int
jdFromDate dd mm yy =
  dd + ((153 * m + 2) `div` 5) + 365 * y + (y `div` 4) - (y `div` 100) + (y `div` 400) - 32045
    where
      a = (14 - mm) `div` 12
      m = mm + 12 * a - 3
      y = yy + 4800 - a

jdToFactors :: Int -> (Int, Int, Int)
jdToFactors jd
  | jd > 2299160 = (a, b, c)
  | otherwise = (-1, 0, d)
    where
      a = jd + 32044
      b = (4 * a + 3) `div` 146097
      c = a - ((b * 146097) `div` 4)
      d = jd + 32082

factorsToDate :: (Int, Int, Int) -> (Int, Int, Int)
factorsToDate (a, b, c) =
  (e - ((153 * m + 2) `div` 5) + 1, m + 3 - 12 * (m `div` 10), b * 100 + d - 4800 + (m `div` 10))
      where
        d = (4 * c + 3) `div` 1461
        e = c - ((1461 * d) `div` 4)
        m = (5 * e + 2) `div` 153

jdToDate :: Int -> (Int, Int, Int)
jdToDate jd = factorsToDate (jdToFactors jd)

calculateT1 :: Int -> Float
calculateT1 k = fromIntegral k / 1236.85

calculateDeltaT :: Float -> Float
calculateDeltaT t1
  | t1 < -11 = 0.001 + 0.000839 * t1 + 0.0002261 * t2 - 0.00000845 * t3 - 0.000000081* t1* t3
  | otherwise = -0.000278 + 0.000265 * t1 + 0.000262 * t2
    where
      t2 = t1 ^ 2
      t3 = t1 ^ 3

calculateLatitude :: Int -> Float
calculateLatitude k =
  c + 0.0010 * sin (dr * (2 * f - mpr)) + 0.0005 * sin (dr * (2 * mpr + m))
    where
      k1 = fromIntegral k
      t1 = calculateT1 k
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

getNewMoonDay :: Int -> Int -> Int
getNewMoonDay k timeZone =
  floor (jdNewMoon + 0.5 + fromIntegral timeZone / 24)
    where
      k1 = fromIntegral k
      dr = pi / 180
      t1 = calculateT1 k
      t2 = t1 ^ 2
      t3 = t1 ^3
      deltaT = calculateDeltaT t1
      c = calculateLatitude k
      jd2 = 2415020.75933 + 29.53058868 * k1 + 0.0001178 * t2 - 0.000000155 * t3
      jd1 = jd2 + 0.00033 * sin ((166.56 + 132.87 * t1 - 0.009173 * t2) * dr)
      jdNewMoon = jd1 + c - deltaT

getSunLongitude :: Int -> Int -> Int
getSunLongitude jd timeZone =
  floor (l / pi * 6)
    where
      dr = pi / 180
      t1 = (fromIntegral jd - 2451545.5 - fromIntegral timeZone / 24) / 36525
      t2 = t1 ^ 2
      m = 357.52910 + 35999.05030 * t1 - 0.0001559 * t2 - 0.00000048 * t1 * t2
      dl1 = (1.914600 - 0.004817 * t1 - 0.000014 * t2) * sin (dr * m)
      dl = dl1 + (0.019993 - 0.000101 * t1) * sin (dr * 2 * m) + 0.000290 * sin (dr * 3 * m)
      l3 = 280.46645 + 36000.76983 * t1 + 0.0003032 * t2
      l2 = l3 + dl
      l1 = l2 * dr
      l = l1 - pi * 2 * fromIntegral (floor (l1 / (pi * 2)))

getLunarMonthEleven :: Int -> Int -> Int
getLunarMonthEleven yy timeZone
  | sunLng >= 9 = getNewMoonDay (k - 1) timeZone
  | otherwise = getNewMoonDay k timeZone
    where
      off = jdFromDate 31 12 yy - 2415021
      k = floor (fromIntegral off / 29.530588853)
      nm = getNewMoonDay k timeZone
      sunLng = getSunLongitude nm timeZone

getLeapMonthOffsetFromFactors :: Int -> Int -> Int -> Int -> Int -> Int
getLeapMonthOffsetFromFactors m11 timeZone last arc index
  | arc /= last && index < 14 = getLeapMonthOffsetFromFactors m11 timeZone tmpLast tmpArc tmpIndex
  | otherwise = index - 1
    where
      k = floor ((fromIntegral m11 - 2415021.076998695) / 29.530588853 + 0.5)
      tmpLast = last
      tmpArc = getSunLongitude (getNewMoonDay (k + index) timeZone) timeZone
      tmpIndex = tmpIndex + 1

getLeapMonthOffset :: Int -> Int -> Int
getLeapMonthOffset m11 timeZone = getLeapMonthOffsetFromFactors m11 timeZone last arc index where
  k = floor ((fromIntegral m11 - 2415021.076998695) / 29.530588853 + 0.5)
  arc = getSunLongitude (getNewMoonDay (k + 1)  timeZone) timeZone
  last = 0
  index = 1

getLunarMonthStart :: (Int, Int, Int, Int) -> Int
getLunarMonthStart (dd, mm, yy, timeZone)
  | monthStart > dayNumber = getNewMoonDay k timeZone
  | otherwise = getNewMoonDay (k + 1) timeZone
    where
      dayNumber = jdFromDate dd mm yy
      k = floor ((fromIntegral dayNumber - 2415021.076998695) / 29.530588853)
      monthStart = getNewMoonDay (k + 1) timeZone

lunarDayFromSolar :: (Int, Int, Int, Int) -> Int
lunarDayFromSolar (dd, mm, yy, timeZone) = dayNumber - monthStart + 1 where
  dayNumber = jdFromDate dd mm yy
  k = floor ((fromIntegral dayNumber - 2415021.076998695) / 29.530588853)
  monthStart = getLunarMonthStart (dd, mm, yy, timeZone)

lunarFactorsFromDate :: (Int, Int, Int, Int) -> (Int, Int)
lunarFactorsFromDate (dd, mm, yy, timeZone)
  | a11 >= monthStart = (getLunarMonthEleven (yy - 1) timeZone, a11)
  | otherwise = (a11, getLunarMonthEleven (yy + 1) timeZone)
    where
      monthStart = getLunarMonthStart (dd, mm, yy, timeZone)
      a11 = getLunarMonthEleven yy timeZone

getLunarMonthWithFactors :: (Int, Int, Int, Int) -> Int
getLunarMonthWithFactors (dd, mm, yy, timeZone)
  | b11 - a11 > 365 && diff >= leapMonthDiff = diff + 10
  | otherwise = diff + 11
    where
      monthStart = getLunarMonthStart (dd, mm, yy, timeZone)
      diff = floor (fromIntegral (monthStart - a11) / 29)
      leapMonthDiff = getLeapMonthOffset a11 timeZone
      (a11, b11) = lunarFactorsFromDate (dd, mm, yy, timeZone)

lunarMonthFromSolar :: (Int, Int, Int, Int) -> Int
lunarMonthFromSolar (dd, mm, yy, timeZone)
  | lunarMonth > 12 = lunarMonth - 12
  | otherwise = lunarMonth
    where
      lunarMonth = getLunarMonthWithFactors (dd, mm, yy, timeZone)

getLunarYearWithFactors :: (Int, Int, Int, Int) -> Int
getLunarYearWithFactors (dd, mm, yy, timeZone)
  | a11 >= monthStart = yy
  | otherwise = yy + 1
    where
      monthStart = getLunarMonthStart (dd, mm, yy, timeZone)
      a11 = getLunarMonthEleven yy timeZone

lunarYearFromSolar :: (Int, Int, Int, Int) -> Int
lunarYearFromSolar (dd, mm, yy, timeZone)
  | lunarMonth >= 11 && diff < 4 = lunarYear - 1
  | otherwise = lunarYear
    where
      lunarMonth = lunarMonthFromSolar (dd, mm, yy, timeZone)
      monthStart = getLunarMonthStart (dd, mm, yy, timeZone)
      diff = floor (fromIntegral (monthStart - a11) / 29)
      lunarYear = getLunarYearWithFactors (dd, mm, yy, timeZone)
      (a11, b11) = lunarFactorsFromDate (dd, mm, yy, timeZone)

isLunarLeapYear :: Int -> Bool
isLunarLeapYear yy = (yy `mod` 19) `elem` [0, 3, 6, 9, 11, 14, 17]

convertSolarToLunar :: (Int, Int, Int, Int) -> (Int, Int, Int)
convertSolarToLunar (dd, mm, yy, timeZone) = (day, month, year) where
  day = lunarDayFromSolar (dd, mm, yy, timeZone)
  month = lunarMonthFromSolar (dd, mm, yy, timeZone)
  year = lunarYearFromSolar (dd, mm, yy, timeZone)

calculateLunarFactorsFromLunarDate :: (Int, Int, Int, Int) -> (Int, Int)
calculateLunarFactorsFromLunarDate (lDay, lMonth, lYear, timeZone)
  | lMonth < 11 =
    (getLunarMonthEleven (lYear - 1) timeZone, getLunarMonthEleven lYear timeZone)
  | otherwise =
    (getLunarMonthEleven lYear timeZone, getLunarMonthEleven (lYear + 1) timeZone)

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
      leapOffset = getLeapMonthOffset a11 timeZone
      (a11, b11) =
        calculateLunarFactorsFromLunarDate (lDay, lMonth, lYear, timeZone)

convertLunarToSolar :: (Int, Int, Int, Int) -> (Int, Int, Int)
convertLunarToSolar (lDay, lMonth, lYear, timeZone) =
  jdToDate (monthStart + lDay - 1)
    where
      (a11, b11) =
        calculateLunarFactorsFromLunarDate (lDay, lMonth, lYear, timeZone)
      k = floor (0.5 + (fromIntegral a11 - 2415021.076998695) / 29.530588853)
      offset = calculateLunarSolarOffset (lDay, lMonth, lYear, timeZone)
      monthStart = getNewMoonDay (k + offset) timeZone
