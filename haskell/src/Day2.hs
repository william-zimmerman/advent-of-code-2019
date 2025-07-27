{-# LANGUAGE ScopedTypeVariables #-}

module Day2 (runDay2) where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Address = Int
type AddressableList a = [(Address, a)]
type AddressableMemory = AddressableList Int

newtype InstructionPointer = MkInstructionPointer Int deriving (Show)

data Instruction = Add InstructionParams3 | Multiply InstructionParams3 | Halt

data InstructionParams3 = MkInstructionParams3
    { inputAddresses :: (Address, Address)
    , outputAddress :: Address
    }

runDay2 :: IO ()
runDay2 = do
    strings <- concatMap (splitOn ",") . lines <$> readFile "resources/day2.txt"
    let ints :: [Int] = map read strings
    let memory = createAddressableList ints
    let answer = [(x, y) | x <- [0 .. 99], y <- [0 .. 99], getAnswer (runProgram (x, y) memory) == Just 19690720]
    print answer
    return ()

runProgram :: (Int, Int) -> AddressableMemory -> AddressableMemory
runProgram initialInputs memory =
    let adjustedMemory = replace 2 (snd initialInputs) (replace 1 (fst initialInputs) memory)
     in recurseProgram start adjustedMemory

recurseProgram :: InstructionPointer -> AddressableMemory -> AddressableMemory
recurseProgram instructionPointer memory =
    let maybeInstruction = getInstruction instructionPointer memory
     in case maybeInstruction of
            Nothing -> trace ("Unable to parse instruction at address " <> show instructionPointer) memory
            Just Halt -> trace ("Program halted from instruction at address " <> show instructionPointer) memory
            Just instruction -> recurseProgram (nextInstructionPointer instruction instructionPointer) (applyInstruction instruction memory)

numberOfParams :: Instruction -> Int
numberOfParams (Add _) = 3
numberOfParams (Multiply _) = 3
numberOfParams Halt = 0

start :: InstructionPointer
start = MkInstructionPointer 0

createAddressableList :: [a] -> AddressableList a
createAddressableList = zip [0 ..]

replace :: Address -> a -> AddressableList a -> AddressableList a
replace _ _ [] = []
replace position newValue ((p', currentValue) : xs)
    | position == p' = (position, newValue) : xs
    | otherwise = (p', currentValue) : replace position newValue xs

getInstruction :: InstructionPointer -> AddressableMemory -> Maybe Instruction
getInstruction instructionPointer@(MkInstructionPointer address) memory =
    let maybeOpcode = lookup address memory
        parseOpcode :: Int -> Maybe Instruction
        parseOpcode 1 = Add <$> getInstructionParams3 instructionPointer memory
        parseOpcode 2 = Multiply <$> getInstructionParams3 instructionPointer memory
        parseOpcode 99 = Just Halt
        parseOpcode _ = Nothing
     in parseOpcode =<< maybeOpcode

getInstructionParams3 :: InstructionPointer -> AddressableMemory -> Maybe InstructionParams3
getInstructionParams3 (MkInstructionPointer address) memory =
    let
        maybeInput1Address = lookup (address + 1) memory
        maybeInput2Address = lookup (address + 2) memory
        maybeOutputAddress = lookup (address + 3) memory
     in
        MkInstructionParams3 <$> ((,) <$> maybeInput1Address <*> maybeInput2Address) <*> maybeOutputAddress

nextInstructionPointer :: Instruction -> InstructionPointer -> InstructionPointer
nextInstructionPointer instruction (MkInstructionPointer address) =
    MkInstructionPointer (address + numberOfParams instruction + 1)

applyInstruction :: Instruction -> AddressableMemory -> AddressableMemory
applyInstruction (Add params) memory = applyInstructionParams3 (+) params memory
applyInstruction (Multiply params) memory = applyInstructionParams3 (*) params memory
applyInstruction Halt memory = memory

applyInstructionParams3 :: (a -> a -> a) -> InstructionParams3 -> AddressableList a -> AddressableList a
applyInstructionParams3 f (MkInstructionParams3 (inputAddress1, inputAddress2) outputAddress') list =
    let maybeInputValues = (,) <$> lookup inputAddress1 list <*> lookup inputAddress2 list
        computedValue = uncurry f (fromJust maybeInputValues)
     in replace outputAddress' computedValue list

getAnswer :: AddressableList a -> Maybe a
getAnswer = lookup 0

-- 509871 is too low
