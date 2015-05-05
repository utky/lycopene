module Lycopene.Core.Project.Value where

data ProjectV = Values String (Maybe String)

tuple :: ProjectV -> (String, Maybe String)
tuple (Values a b) = (a, b)
