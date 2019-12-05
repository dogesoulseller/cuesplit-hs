module LineTypeCheck (isRemark, isCommand', isCommand) where
import Data.Char(isSpace)

isRemark :: String -> Bool
isRemark [] = True
isRemark (c:rest)
  | c == 'R' && take 2 rest == "EM" = True -- If contains REM, it's a remark
  | isSpace c = isRemark rest -- If is whitespace, continue parsing
  | otherwise = False -- If character is not whitespace and REM hasn't occured yet, it's not a remark

isCommand' :: String -> Int -> String -> Bool
isCommand' search searchLen = go
  where
    go [] = False
    go (c:rest)
      | c == head search && take (searchLen -1) rest == tail search = True
      | isSpace c = go rest
      | otherwise = False

isCommand :: String -> String -> Bool
isCommand search = go
  where
    go [] = False
    go (c:rest)
      | c == head search && take (length $ tail search) rest == tail search = True
      | isSpace c = go rest
      | otherwise = False
