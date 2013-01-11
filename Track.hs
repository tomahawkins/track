module Main (main) where

import Data.List (nubBy, intercalate)

data Segment = Straight | LeftTurn | RightTurn deriving (Show, Read, Eq)

type Track = [Segment]

type Position = (Double, Double, Double)   -- X, Y, Azmith

main :: IO ()
main = do
  tracks <- readFile "tracks" >>= return . read
  sequence_ [ writeFile ("track" ++ show i ++ ".svg") $ renderTrack t | (t, i) <- zip tracks [1 ..] ]

-- Renders a track to svg.
renderTrack :: Track -> String
renderTrack track = unlines
  [ "<?xml version=\"1.0\"?>"
  , "<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\""
  , "  \"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">"
  , "<svg xmlns=\"http://www.w3.org/2000/svg\" version=\"1.1\">"
  , "  <path d=\"" ++ path ++ "\" stroke=\"blue\" fill=\"none\" />"
  , unlines [ "  <circle cx=\"" ++ show x ++ "\" cy=\"" ++ show y ++ "\" r=\"3\" fill=\"red\" />" | (x, y) <- tail coords ]
  , "</svg>"
  ]
  where
  states = scanl next (0, 0, 0) track
  (xs, ys) = unzip [ (x, y) | (x, y, _) <- states ]
  coords = [ (100 + 100 * (x - minimum xs), 100 + 100 * (y - minimum ys)) | (x, y, _) <- states ]
  path = "M " ++ show x ++ " " ++ show y ++ " " ++ intercalate " " (map format $ zip track rest)
    where
    (x, y) = head coords
    rest = tail coords
  format :: (Segment, (Double, Double)) -> String
  format (a, (x, y)) = case a of
    Straight  -> "L " ++ show x ++ " " ++ show y
    LeftTurn  -> "A 100 100 0 0 1 " ++ show x ++ " " ++ show y
    RightTurn -> "A 100 100 0 0 0 " ++ show x ++ " " ++ show y
 
-- Computes the next position and heading given a new track segment.
next :: Position -> Segment -> Position
next (x, y, o) a = case a of
  Straight  -> (x + cos o, y + sin o, o)
  LeftTurn  -> (x + tf * cos o - ts * sin o, y + tf * sin o + ts * cos o, o + pi / 4)
  RightTurn -> (x + tf * cos o + ts * sin o, y + tf * sin o - ts * cos o, o - pi / 4)
  where
  tf = sin $ pi / 4
  ts = 1 - cos (pi / 4)

-- Check to see if a track is valid, i.e. it is closed and uses the available number of segments.
validTrack :: Track -> Bool
validTrack track = length track <= 16 && straights <= 4 && turns <= 12 && closed
  where
  (x, y, o) = foldl next (0, 0, 0) track
  closed = angle0 o && abs x < 0.05 && abs y < 0.05
  straights  = length [ () | Straight  <- track ]
  leftTurns  = length [ () | LeftTurn  <- track ]
  rightTurns = length [ () | RightTurn <- track ]
  turns      = leftTurns + rightTurns

  angle0 :: Double -> Bool
  angle0 a = a2 < 0.05
    where
    a0 = abs $ a / (2 * pi)
    a1 = fromIntegral $ floor a0
    a2 = a0 - a1

-- Enumerate all possible tracks, filter out the ones that are redundant or invalid.
tracks :: Int -> [Track]
tracks n = nubBy tracksEqual $ filter validTrack $ sequence (replicate n [Straight, LeftTurn, RightTurn])

-- Testing equality of two tracks.
tracksEqual :: Track -> Track -> Bool
tracksEqual a b = match (perm b)
               || match (perm $ reverse b)
               || match (perm $ inverse b)
               || match (perm $ reverse $ inverse b)
  where
  match bs = any (a ==) bs
  perm t = [ take (length t) $ drop n $ t ++ t | n <- [0 .. length t - 1] ]
  inverse = map inv
  inv a = case a of
    Straight  -> Straight
    LeftTurn  -> RightTurn
    RightTurn -> LeftTurn


