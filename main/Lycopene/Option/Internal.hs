module Lycopene.Option.Internal where

import           Options.Applicative


-- | Reads all arguments as one text.
argtext :: Parser String
argtext = concat <$> many
  ( argument str
    (  metavar "TEXT..."
    <> help    "")
  )

-- | Read multi line string as description.
-- If it's not present parser tries to read from STDIN.
description :: Parser String
description = option str
  (  long    "description"
  <> short   'd'
  <> metavar "FILE"
  <> help    "")

-- |
name :: Parser String
name = option str
  (  long    "name"
  <> short   'n'
  <> metavar "NAME"
  <> help    "")

-- |
subject :: Parser String
subject = option str
  (  long    "subject"
  <> short   's'
  <> metavar "SUBJECT"
  <> help    "")

argid :: Parser Integer
argid = argument auto
  ( metavar "ID"
  <> help    "")

