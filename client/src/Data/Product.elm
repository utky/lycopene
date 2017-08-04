module Data.Product exposing (..)

type Product a b = Product a b

wrap : (a, b) -> Product a b
wrap (x, y) = Product x y

unwrap : Product a b -> (a, b)
unwrap (Product x y) = (x, y)

bimap : (a -> c) -> (b -> d) -> Product a b -> Product c d
bimap f g (Product x y) = Product (f x) (g y)

tmap : (a -> c) -> (b -> d) -> (a, b) -> (c, d)
tmap f g = unwrap << bimap f g << wrap

lmap : (a -> c) -> Product a b -> Product c b
lmap f (Product x y) = Product (f x) y

rmap : (b -> d) -> Product a b -> Product a d
rmap f (Product x y) = Product x (f y)
