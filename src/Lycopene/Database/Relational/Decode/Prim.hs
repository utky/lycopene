module Lycopene.Database.Relational.Decode.Prim where

import           Control.Monad.Reader

type Decode a b = ReaderT a DecodeResult b

type DecodeResult = Either DecodeError

data DecodeError = DecodeFail String deriving (Show)

execDecode :: Decode a b -> a -> DecodeResult b
execDecode = runReaderT

decoder' :: (a -> DecodeResult b) -> Decode a b
decoder' = ReaderT

decoder :: (a -> b) -> Decode a b
decoder f = decoder' $ Right . f

failure :: String -> DecodeResult a
failure = Left . DecodeFail

option :: String -> Decode a (Maybe b) -> Decode a b
option s x = x >>= option'
  where
    option' (Just y) = decoder' (const (Right y))
    option' Nothing = decoder' (const (Left (DecodeFail s)))

(<?>) :: Decode a (Maybe b) -> String -> Decode a b
(<?>) = flip option

-- class Decodable a b where
--   decode :: a -> DecodeResult b
