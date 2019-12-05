module LineParse (getQuoted, getUnquoted, getTrackField, isQuoted, findFirst, findLast, slice, TrackFieldType(..))
where

import LineTypeCheck

data TrackFieldType = TrackTitle | TrackArtist | TrackIdx | TrackPregap | TrackTimestamp deriving (Enum)

isQuoted :: String -> Bool
isQuoted = elem '"'

getQuoted :: String -> String
getQuoted str =
  slice startPos endPos str
  where
  startPos = findFirst '"' str + 1
  endPos = findLast '"' str

getUnquoted :: String -> String
getUnquoted str = words str !! 1

getTrackField :: TrackFieldType -> [String] -> String
getTrackField ft = go
  where
  go [] = ""
  go (l:ls) =
    case ft of
      TrackTitle -> if isCommand' "TITLE" 5 l then getQOrUnQ l else go ls
      TrackArtist -> if isCommand' "PERFORMER" 9 l then getQOrUnQ l else go ls
      TrackIdx -> head . tail . words $ l
      TrackPregap -> if isCommand' "INDEX 00" 8 l then words l !! 2 else go ls
      TrackTimestamp -> if isCommand' "INDEX 01" 8 l then words l !! 2 else go ls

getQOrUnQ :: String -> String
getQOrUnQ s = if isQuoted s then getQuoted s else getUnquoted s

findFirst :: Char -> String -> Int
findFirst c str = go str 0
  where
  go [] res = res
  go (s:rest) res
    | s == c = res
    | otherwise = go rest $ res + 1

findLast :: Char -> String -> Int
findLast c str = go str 0 0
  where
  go [] _ res = res
  go (s:rest) acc res
    | s == c = go rest (acc+1) acc
    | otherwise = go rest (acc+1) res

slice :: Int -> Int -> [a] -> [a]
slice start end xs = take (end - start) (drop start xs)
