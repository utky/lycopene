{-# LANGUAGE OverloadedStrings #-}
module Lycopene.Core.Identifier 
    ( IdGen
    , NameIdGen
    , generate
    , nameIdGen
    ) where

import           Lycopene.Core.Scalar (Identifier, Name)
import           Data.Word
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import           Data.UUID (UUID)
import           Data.UUID.V5 (generateNamed, namespaceURL)

-- | Super class which specifies namespace
type Domain = String
-- | Sub class which specifies responsive element
type Kingdom = String

schema :: [Word8]
schema = B.unpack "lycopene://"

host :: [Word8]
host = B.unpack "ilyaletre"

domain :: Domain -> [Word8]
domain = B.unpack . C.pack . ("/" ++ )

kingdom :: Kingdom -> [Word8]
kingdom = B.unpack . C.pack . ("/" ++ )

namedURL :: (Domain, Name) -> [Word8]
namedURL (ns, n) = schema ++ host ++ (domain ns) ++ (kingdom n)

data IdGen s = IdGen (s -> Identifier)

generate :: IdGen s -> s -> Identifier
generate (IdGen g) a = g a

type NameIdGen = IdGen (Domain, Name)

nameIdGen :: NameIdGen
nameIdGen = IdGen (generateNamed namespaceURL . namedURL)
