module AddressableList (Address, AddressableList, create, replace, eitherLookup) where

import Text.Printf (printf)

type Address = Int
type AddressableList a = [(Address, a)]

create :: [a] -> AddressableList a
create = zip [0 ..]

replace :: Address -> a -> AddressableList a -> AddressableList a
replace _ _ [] = []
replace position newValue ((p', currentValue) : xs)
    | position == p' = (position, newValue) : xs
    | otherwise = (p', currentValue) : replace position newValue xs

eitherLookup :: Address -> AddressableList a -> Either String a
eitherLookup address memory = maybe (Left (printf "Unable to read memory at address %i" (show address))) Right (lookup address memory)
