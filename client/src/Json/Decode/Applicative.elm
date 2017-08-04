module Json.Decode.Applicative exposing (..)

import Json.Decode as Decode exposing (Decoder)

(|$|) : (a -> b) -> Decoder a -> Decoder b
(|$|) = Decode.map

infixl 9 |$|

(|*|) : Decoder (a -> b) -> Decoder a -> Decoder b
(|*|) = Decode.map2 (<|)

infixl 9 |*|

