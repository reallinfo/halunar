# Halunar

A Lunar calendar calculator library written in Haskell.

### Usage

```hs
calculateLunarDateFromDate :: (Int, Int, Int, Int) -> (Int, Int, Int)
calculateLunarDateFromDate (day, month, year, timeZone) = (lunarDay, lunarMonth, lunarYear)
```

```hs
calculateDateFromLunarDate :: (Int, Int, Int, Int) -> (Int, Int, Int)
calculateDateFromLunarDate (lunarDay, lunarMonth, lunarYear, timeZone) = (day, month, year)
```

### License

[MIT](LICENSE)

### Reference

[Computing the Vietnamese lunar calendar](https://www.informatik.uni-leipzig.de/~duc/amlich/calrules_en.html)
