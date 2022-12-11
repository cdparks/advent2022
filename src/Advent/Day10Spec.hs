module Advent.Day10Spec
  ( spec
  ) where

import Advent.Prelude hiding (cycle)

import Advent.Input
import Advent.Parse hiding (number, takeWhile)
import Advent.Vec2 (Vec2, zero)
import qualified Advent.Vec2 as Vec2
import qualified Data.HashSet as HashSet

spec :: Spec
spec = parsing parseProgram 10 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 13140
    part1 problem `shouldBe` 13440

  it "2" $ \Input{..} -> do
    render (part2 example)
      `shouldBe` unlines
        [ "██  ██  ██  ██  ██  ██  ██  ██  ██  ██  "
        , "███   ███   ███   ███   ███   ███   ███ "
        , "████    ████    ████    ████    ████    "
        , "█████     █████     █████     █████     "
        , "██████      ██████      ██████      ████"
        , "███████       ███████       ███████     "
        ]
    render (part2 problem)
      `shouldBe` unlines
        [ "███  ███  ████  ██  ███   ██  ████  ██  "
        , "█  █ █  █    █ █  █ █  █ █  █    █ █  █ "
        , "█  █ ███    █  █    █  █ █  █   █  █  █ "
        , "███  █  █  █   █ ██ ███  ████  █   ████ "
        , "█    █  █ █    █  █ █ █  █  █ █    █  █ "
        , "█    ███  ████  ███ █  █ █  █ ████ █  █ "
        ]

-- | Sample the signal strength on interesting cycles
part1 :: [Instruction] -> Int
part1 = runWith 0 $ \cpu -> sample . exec cpu
 where
  sample :: CPU Int -> CPU Int
  sample cpu
    | cpu ^. cycle `elem` targets = cpu & port +~ cpu ^. cycle * cpu ^. regX
    | otherwise = cpu

  targets :: [Int]
  targets = [20, 60, 100, 140, 180, 220]

-- | Illuminate each pixel when it's overlapped by the sprite
part2 :: [Instruction] -> Screen
part2 = runWith (Screen zero mempty) $ exec . draw
 where
  draw :: CPU Screen -> CPU Screen
  draw cpu = cpu
    & port %~ advance
    . illuminate (cpu ^. regX)

  illuminate :: Int -> Screen -> Screen
  illuminate x screen
    | screen ^. pos . Vec2.x `elem` [pred x, x, succ x] = screen
      & lit %~ HashSet.insert (screen ^. pos)
    | otherwise = screen

  advance :: Screen -> Screen
  advance screen
    | screen ^. pos . Vec2.x == bounds ^. Vec2.x = screen
      & (pos . Vec2.x .~ 0)
      . (pos . Vec2.y +~ 1)
    | otherwise = screen & pos +~ Vec2.right

-- | Install state, run instructions with, and extract state
runWith :: port -> (CPU port -> Instruction -> CPU port) -> [Instruction] -> port
runWith p step = _port . foldl' step (newCPU p) . expand
{-# INLINE runWith #-}

-- | New CPU with some state
newCPU :: port -> CPU port
newCPU = CPU 1 1
{-# INLINE newCPU #-}

-- | Execute a single instruction
exec :: CPU port -> Instruction -> CPU port
exec cpu = \case
  Noop -> cpu & cycle +~ 1
  AddX i -> cpu & (cycle +~ 1) . (regX +~ i)
{-# INLINE exec #-}

-- | Render 'Screen' state to text
render :: Screen -> Text
render screen = pack $ do
  y <- [0.. bounds ^. Vec2.y]
  x <- [0.. bounds ^. Vec2.x]
  if Vec2.new x y `HashSet.member` (screen ^. lit)
    then '█' : newline x
    else ' ' : newline x
 where
  newline x
    | x == bounds ^. Vec2.x = "\n"
    | otherwise = ""

-- | Screen bounds
bounds :: Vec2 Int
bounds = Vec2.new 39 5
{-# INLINE bounds #-}

-- | CPU tracks cycle, register, and some other state
data CPU port = CPU
  { _cycle :: {-# UNPACK #-} !Int
  , _regX :: {-# UNPACK #-} !Int
  , _port :: port
  }
  deriving (Eq, Show)

cycle :: Lens' (CPU port) Int
cycle = lens _cycle $ \s c -> s { _cycle = c }
{-# INLINE cycle #-}

regX :: Lens' (CPU port) Int
regX = lens _regX $ \s r -> s { _regX = r }
{-# INLINE regX #-}

port :: Lens (CPU port) (CPU port') port port'
port = lens _port $ \s p -> s { _port = p }
{-# INLINE port #-}

-- | Screen has a beam position and the pixels that have been illuminated
data Screen = Screen
  { _pos :: Vec2 Int
  , _lit :: HashSet (Vec2 Int)
  }
  deriving (Eq, Show)

pos :: Lens' Screen (Vec2 Int)
pos = lens _pos $ \s p -> s { _pos = p }
{-# INLINE pos #-}

lit :: Lens' Screen (HashSet (Vec2 Int))
lit = lens _lit $ \s l -> s { _lit = l }
{-# INLINE lit #-}

-- | Instruction set
data Instruction
  = Noop
  -- ^ No effect
  | AddX Int
  -- ^ Accumulate argument in X
  deriving (Eq, Show)

-- | Add a noop before 'AddX' to simulate waiting a cycle
expand :: [Instruction] -> [Instruction]
expand = concatMap $ \case
  Noop -> pure Noop
  AddX i -> [Noop, AddX i]

-- | Parse one instruction per line
parseProgram :: Parser [Instruction]
parseProgram = parseInstruction `sepBy1` endOfLine

-- | @noop@ or @addx@ w/ a signed argument
parseInstruction :: Parser Instruction
parseInstruction = asum
  [ Noop <$ string "noop"
  , AddX <$ sym "addx" <*> signed decimal
  ]
