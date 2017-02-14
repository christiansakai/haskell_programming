import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate   UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
  [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldl getUTCTime []
  where getUTCTime acc (DbDate utcTime) = utcTime : acc
        getUTCTime acc _                = acc

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldl getNumber []
  where getNumber acc (DbNumber number) = number : acc
        getNumber acc _                 = acc

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldl getRecentDate (UTCTime (fromGregorian 1900 1 1) (secondsToDiffTime 34123))
  where getRecentDate acc (DbDate utcTime)
                    | acc < utcTime = utcTime
                    | otherwise     = acc
        getRecentDate acc _         = acc

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr sumNumber 0
  where sumNumber (DbNumber number) acc = number + acc
        sumNumber _                 acc = acc

avgDb :: [DatabaseItem] -> Double
avgDb ds = let (total, count) = foldr getTotalCount (0, 0) ds
                              where getTotalCount (DbNumber number) (total, count) = (number + total, count + 1)
                                    getTotalCount _                 acc            = acc
            in (fromIntegral total) / (fromIntegral count)
