module Advent.Day07Spec
  ( spec
  ) where

import Advent.Prelude hiding (Down)

import Advent.Input
import Advent.Parse
import Advent.Trie (Trie)
import qualified Advent.Trie as Trie
import Data.Monoid (Sum(..))

spec :: Spec
spec = parsing (interpret <$> parseCommands) 7 $ do
  it "1" $ \Input{..} -> do
    part1 example `shouldBe` 95437
    part1 problem `shouldBe` 1391690

  it "2" $ \Input{..} -> do
    part2 example `shouldBe` 24933642
    part2 problem `shouldBe` 5469168

-- | Sum of size of directories <= 100000
part1 :: Trie String Int -> Int
part1 t = sum [size | size <- toList t, size <= 100000]

-- | Size of smallest directory to delete to free up enough space
part2 :: Trie String Int -> Int
part2 t =
  minimum [size | size <- toList t, size >= need]
 where
  total = fromMaybe 0 $ Trie.value t
  need = 30000000 - (70000000 - total)

-- | Move up, down into directory, or list files with these sizes
data Command
  = Up
  | Down String
  | List [Int]
  deriving (Eq, Show)

-- | File system is a path trie and a current working directory
data FS = FS
  { _trie :: Trie String (Sum Int)
  , _path :: [String]
  }

-- | Read file sizes into a 'Trie' mapping directory paths to their total size
interpret :: [Command] -> Trie String Int
interpret
  = fmap getSum
  . Trie.flowUp
  . _trie
  . flip execState (FS mempty mempty)
  . traverse step
 where
  step = \case
    Up ->
      modify $ \fs@FS{..} -> fs
        { _path = drop 1 _path
        }
    Down name ->
      modify $ \fs@FS{..} -> fs
        { _path = name : _path
        }
    List files -> do
      let size = foldMap Sum files
      modify $ \fs@FS{..} -> fs
        { _trie = Trie.insert (reverse _path) size _trie
        }

-- | Parse at least one command
parseCommands :: Parser [Command]
parseCommands = some parseCommand

-- | Change working directory or list its files
parseCommand :: Parser Command
parseCommand = sym "$" *> (parseNavigate <|> parseList)

-- | Change working directory
parseNavigate :: Parser Command
parseNavigate = sym "cd" *> asum
  [ Up <$ sym ".."
  , Down <$> asum
    [ token $ pure <$> char '/'
    , token $ some letter
    ]
  ]

-- | List working directory's files
parseList :: Parser Command
parseList = List <$ sym "ls" <*> fmap catMaybes (some parseNode)

-- | We only care about the the leaf-level file sizes
--
-- We'll get directory structure from 'interpret' later.
--
parseNode :: Parser (Maybe Int)
parseNode = asum
  [ Just <$> parseFileSize
  , Nothing <$ parseDirectory
  ]

-- | Parse directory then throw it away
parseDirectory :: Parser ()
parseDirectory = void $ sym "dir" *> token (some letter)

-- | Parse size then filename
parseFileSize :: Parser Int
parseFileSize = token decimal <* token (some $ letter <|> char '.')
