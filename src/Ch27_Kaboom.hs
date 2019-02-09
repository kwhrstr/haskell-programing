module Ch27_Kaboom where

import Prelude  hiding (foldr)

possiblyKaboom f = f fst snd (0, undefined)

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr k z [] = z
myFoldr k z (x:xs) = k x $ myFoldr k z xs

c = myFoldr const 'z' ['a' .. 'e']

    