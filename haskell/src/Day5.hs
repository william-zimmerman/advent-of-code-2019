{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (runDay5) where

import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Address = Int
type AddressableList a = [(Address, a)]
type AddressableMemory = AddressableList Int

data ParameterMode = Position | Immediate

type InstructionParameter = Int
type InstructionParameterTriple = (InstructionParameter, InstructionParameter, InstructionParameter)

newtype InstructionPointer = MkInstructionPointer Int deriving (Show)

data Instruction = Add InstructionParameterTriple | Multiply InstructionParameterTriple | Halt

runDay5 :: IO ()
runDay5 = do
    strings <- concatMap (splitOn ",") . lines <$> readFile "resources/day5.txt"
    let ints :: [Int] = map read strings
    let memory = createAddressableList ints
    print memory
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

getInstructionParams3 :: InstructionPointer -> AddressableMemory -> Maybe InstructionParameterTriple
getInstructionParams3 (MkInstructionPointer address) memory =
    let
        maybeInput1Address = lookup (address + 1) memory
        maybeInput2Address = lookup (address + 2) memory
        maybeOutputAddress = lookup (address + 3) memory
     in
        (,,) <$> maybeInput1Address <*> maybeInput2Address <*> maybeOutputAddress

nextInstructionPointer :: Instruction -> InstructionPointer -> InstructionPointer
nextInstructionPointer instruction (MkInstructionPointer address) =
    MkInstructionPointer (address + numberOfParams instruction + 1)

applyInstruction :: Instruction -> AddressableMemory -> AddressableMemory
applyInstruction (Add params) memory = applyInstructionParams3 (+) params memory
applyInstruction (Multiply params) memory = applyInstructionParams3 (*) params memory
applyInstruction Halt memory = memory

applyInstructionParams3 :: (a -> a -> a) -> InstructionParameterTriple -> AddressableList a -> AddressableList a
applyInstructionParams3 f (inputAddress1, inputAddress2, outputAddress') list =
    let maybeInputValues = (,) <$> lookup inputAddress1 list <*> lookup inputAddress2 list
        computedValue = uncurry f (fromJust maybeInputValues)
     in replace outputAddress' computedValue list

getAnswer :: AddressableList a -> Maybe a
getAnswer = lookup 0

-- 509871 is too low
