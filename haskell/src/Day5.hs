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

data Instruction = Add | Multiply | Halt

type InstructionAndParameters = (Instruction, [InstructionParameter])

numberOfParams :: Instruction -> Int
numberOfParams Add = 3
numberOfParams Multiply = 3
numberOfParams Halt = 0

runProgram :: (Int, Int) -> AddressableMemory -> AddressableMemory
runProgram initialInputs memory =
    let adjustedMemory = replace 2 (snd initialInputs) (replace 1 (fst initialInputs) memory)
     in recurseProgram start adjustedMemory

recurseProgram :: InstructionPointer -> AddressableMemory -> AddressableMemory
recurseProgram instructionPointer memory =
    let maybeInstructionAndParams = getInstructionAndParams instructionPointer memory
     in case maybeInstructionAndParams of
            Nothing -> trace ("Unable to parse instruction at address " <> show instructionPointer) memory
            Just (Halt, _) -> trace ("Program halted from instruction at address " <> show instructionPointer) memory
            Just instructionAndParams@(instruction, _) -> recurseProgram (nextInstructionPointer instruction instructionPointer) (applyInstruction instructionAndParams memory)

start :: InstructionPointer
start = MkInstructionPointer 0

createAddressableList :: [a] -> AddressableList a
createAddressableList = zip [0 ..]

replace :: Address -> a -> AddressableList a -> AddressableList a
replace _ _ [] = []
replace position newValue ((p', currentValue) : xs)
    | position == p' = (position, newValue) : xs
    | otherwise = (p', currentValue) : replace position newValue xs

getInstructionAndParams :: InstructionPointer -> AddressableMemory -> Maybe InstructionAndParameters
getInstructionAndParams instructionPointer@(MkInstructionPointer address) memory = do
    opcode <- lookup address memory
    instruction <- parseOpcode opcode
    parameters <- getInstructionParams instruction instructionPointer memory
    return (instruction, parameters)

parseOpcode :: Int -> Maybe Instruction
parseOpcode 1 = Just Add
parseOpcode 2 = Just Multiply
parseOpcode 99 = Just Halt
parseOpcode _ = Nothing

getInstructionParams :: Instruction -> InstructionPointer -> AddressableMemory -> Maybe [InstructionParameter]
getInstructionParams instruction (MkInstructionPointer address) memory =
    let
        getInstructionParams' :: Int -> Address -> AddressableMemory -> [Maybe InstructionParameter]
        getInstructionParams' 0 _ _ = []
        getInstructionParams' count currentAddress memory =
            lookup currentAddress memory : getInstructionParams' (count - 1) (currentAddress + 1) memory
     in
        sequence $ getInstructionParams' (numberOfParams instruction) (address + 1) memory

nextInstructionPointer :: Instruction -> InstructionPointer -> InstructionPointer
nextInstructionPointer instruction (MkInstructionPointer address) =
    MkInstructionPointer (address + numberOfParams instruction + 1)

applyInstruction :: InstructionAndParameters -> AddressableMemory -> AddressableMemory
applyInstruction (Add, params) memory = applyInstructionParams3 (+) params memory
applyInstruction (Multiply, params) memory = applyInstructionParams3 (*) params memory
applyInstruction (Halt, _) memory = memory

applyInstructionParams3 :: (a -> a -> a) -> [InstructionParameter] -> AddressableList a -> AddressableList a
applyInstructionParams3 f parameters list =
    let inputAddress1 = head parameters
        inputAddress2 = parameters !! 1
        outputAddress = parameters !! 2
        maybeInputValues = (,) <$> lookup inputAddress1 list <*> lookup inputAddress2 list
        computedValue = uncurry f (fromJust maybeInputValues)
     in replace outputAddress computedValue list

getAnswer :: AddressableList a -> Maybe a
getAnswer = lookup 0

runDay5 :: IO ()
runDay5 = do
    strings <- concatMap (splitOn ",") . lines <$> readFile "resources/day2.txt"
    let ints :: [Int] = map read strings
    let memory = createAddressableList ints
    print (runProgram (12, 2) memory)
    return ()
