module Cueparse(parseCuesheet) where

import LineTypeCheck
import LineParse
import Data.Char(isSpace)

data CueTRACK = CueTRACK {
  trackTitle :: String
  , trackArtist :: String
  , trackIndex :: Int
  , trackStartTimestamp :: String
  , trackPregapTimestamp :: String
  } deriving (Show)

data CueFILE = CueFILE {
  fileTitle :: String
  , filePath :: String
  , fileArtist :: String
  , fileTracks :: [CueTRACK]
  } deriving (Show)

--TODO: Split cuesheet into per-FILE and per-TRACK chunks
parseCuesheet :: String -> [CueFILE]
parseCuesheet str =
  let noRemarks = map (dropWhile isSpace) $ removeRemarks (lines str)
      trackBoundPairs = formBounds (reverse $ getCommandPositions (isCommand' "TRACK" 5) noRemarks) $ length noRemarks
      fileBoundPairs = formBounds (reverse $ getCommandPositions (isCommand' "FILE" 4) noRemarks) $ length noRemarks
      tracks = map makeTrack $ splitByBounds noRemarks trackBoundPairs
      perFileTracks = reverse $ setTracksToPerFile tracks fileBoundPairs trackBoundPairs
  in
    makeFiles (splitByBounds noRemarks fileBoundPairs) perFileTracks

makeFiles :: [[String]] -> [[CueTRACK]] -> [CueFILE]
makeFiles [] _ = []
makeFiles _ [] = []
makeFiles (str:strs) (track:tracks) = makeFile str track : makeFiles strs tracks

-- TODO: Fix incorrect bounds search for TRACK - goes TRACK to TRACK - should go TRACK to FILE
-- Afterwards remove the -2
setTracksToPerFile :: [CueTRACK] -> [(Int,Int)] -> [(Int,Int)] -> [[CueTRACK]]
setTracksToPerFile tracks fBounds tBounds = go fBounds tBounds 0 [] []
  where
  go fs ts pos res restotal
    | null fs = restotal -- If out of files, end
    | null ts = go (tail fs) tBounds 0 [] (res:restotal) -- If out of tracks, move to another file
    | fst (head ts) >= fst (head fs) && snd (head ts) - 2 <= snd (head fs) =
        go fs (tail ts) (pos+1) (res ++ [tracks !! pos]) restotal -- If file contains track, append and move to another track
    | otherwise = go fs (tail ts) (pos+1) res restotal -- If file doesn't contain this track, try another track

makeFile :: [String] -> [CueTRACK] -> CueFILE
makeFile str = CueFILE (getTrackField TrackTitle str)
  (if isQuoted $ head str then getQuoted $ head str else getUnquoted $ head str)
  (getTrackField TrackArtist str)

makeTrack :: [String] -> CueTRACK
makeTrack str = CueTRACK (getTrackField TrackTitle str) (getTrackField TrackArtist str)
  (read $ getTrackField TrackIdx str) (getTrackField TrackTimestamp str) (getTrackField TrackPregap str)

removeRemarks :: [String] -> [String]
removeRemarks = filter $ not . isRemark

getCommandPositions :: (String -> Bool) -> [String] -> [Int]
getCommandPositions matchFunction strings = go strings 0 []
  where
  go [] _ res = res
  go ss pos res
    | matchFunction (head ss) = go (tail ss) (pos+1) (pos : res) -- Is matching line
    | otherwise = go (tail ss) (pos+1) res -- Is not matching line

formBounds :: [Int] -> Int -> [(Int,Int)]
formBounds bounds final = go bounds []
  where
  go [] res = res
  go bound res
    | length bound > 1 = (head bound, head $ tail bound) : go (tail bound) res
    | otherwise = (head bound, final) : res

splitByBounds :: [String] -> [(Int,Int)] -> [[String]]
splitByBounds fLines bounds = go bounds []
  where
  go [] res = res
  go (b:bs) res =
    uncurry slice b fLines : go bs res
