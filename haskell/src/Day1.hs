module Day1 (part1) where

part1 :: IO ()
part1 = do
  masses <- map read . lines <$> readFile "resources/day1.txt"
  let answer = sum $ map calculateFuelRequirementRecursive masses
  print answer
  return ()

calculateFuelRequirement :: Int -> Int
calculateFuelRequirement moduleMass = (moduleMass `div` 3) - 2

calculateFuelRequirementRecursive :: Int -> Int
calculateFuelRequirementRecursive mass
  | fuelRequirement <= 0 = 0
  | otherwise = fuelRequirement + calculateFuelRequirementRecursive fuelRequirement
  where
    fuelRequirement = (mass `div` 3) - 2
