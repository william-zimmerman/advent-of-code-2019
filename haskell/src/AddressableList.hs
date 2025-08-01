module AddressableList (Address, AddressableList, create, replace) where

type Address = Int
type AddressableList a = [(Address, a)]

create :: [a] -> AddressableList a
create = zip [0 ..]

replace :: Address -> a -> AddressableList a -> AddressableList a
replace _ _ [] = []
replace position newValue ((p', currentValue) : xs)
    | position == p' = (position, newValue) : xs
    | otherwise = (p', currentValue) : replace position newValue xs
