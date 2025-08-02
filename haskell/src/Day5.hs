{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (runDay5) where

import qualified AddressableList as AL (Address, AddressableList, create, replace)
import Data.List (uncons)
import Data.List.Split (splitOn)
import Debug.Trace (trace)
import qualified Opcode as O (
    Instruction (Add, Halt, Multiply, Read, Write),
    Opcode,
    ParameterMode (Immediate, Position),
    defaultMode,
    instruction,
    orderedParameterModes,
    parseOpcodeValue,
 )

type AddressableMemory = AL.AddressableList Int
data InstructionParameter = Immediate Int | Position Int deriving (Show)
newtype InstructionPointer = MkInstructionPointer Int deriving (Show)
type InstructionAndParameters = (O.Instruction, [InstructionParameter])

newtype StdIn = MkStdIn [Int] deriving (Show)
newtype StdOut = MkStdOut [Int] deriving (Show)
data ApplicationState = MkState AddressableMemory StdIn StdOut deriving (Show)

read :: StdIn -> (Maybe Int, StdIn)
read (MkStdIn []) = (Nothing, MkStdIn [])
read (MkStdIn (x : xs)) = (Just x, MkStdIn xs)

write :: Int -> StdOut -> StdOut
write x (MkStdOut xs) = MkStdOut (xs <> [x])

updateMemory :: AddressableMemory -> ApplicationState -> ApplicationState
updateMemory newMemory (MkState _ stdIn stdOut) = MkState newMemory stdIn stdOut

numberOfParams :: O.Instruction -> Int
numberOfParams O.Add = 3
numberOfParams O.Multiply = 3
numberOfParams O.Halt = 0
numberOfParams O.Read = 1
numberOfParams O.Write = 1

runProgram :: (Int, Int) -> ApplicationState -> Maybe ApplicationState
runProgram initialInputs (MkState memory stdIn stdOut) =
    let adjustedMemory = AL.replace 2 (snd initialInputs) (AL.replace 1 (fst initialInputs) memory)
     in recurseProgram start (MkState memory stdIn stdOut)

recurseProgram :: InstructionPointer -> ApplicationState -> Maybe ApplicationState
recurseProgram instructionPointer state@(MkState memory _ _) = do
    (instruction, parameterAndModes) <- trace (show memory) (getInstructionAndParams instructionPointer memory)
    inputs <- trace (show (instruction, parameterAndModes)) (resolveParameters parameterAndModes memory)
    case instruction of
        O.Halt -> Just state
        actionableInstruction -> recurseProgram (nextInstructionPointer instruction instructionPointer) (applyInstruction actionableInstruction inputs state)

start :: InstructionPointer
start = MkInstructionPointer 0

getInstructionAndParams :: InstructionPointer -> AddressableMemory -> Maybe InstructionAndParameters
getInstructionAndParams instructionPointer@(MkInstructionPointer address) memory = do
    opcodeValue <- lookup address memory
    opcode <- O.parseOpcodeValue opcodeValue
    parameters <- getInstructionParams opcode instructionPointer memory
    return (O.instruction opcode, parameters)

getInstructionParams :: O.Opcode -> InstructionPointer -> AddressableMemory -> Maybe [InstructionParameter]
getInstructionParams opcode (MkInstructionPointer address) memory =
    let
        getInstructionParams' :: Int -> [O.ParameterMode] -> AL.Address -> AddressableMemory -> [Maybe InstructionParameter]
        getInstructionParams' 0 _ _ _ = []
        getInstructionParams' count modes currentAddress memory' =
            let maybeHeadAndTail = uncons modes
                currentMode = maybe O.defaultMode fst maybeHeadAndTail
                nextModes = maybe [] snd maybeHeadAndTail
             in fmap (translate currentMode) (lookup currentAddress memory') : getInstructionParams' (count - 1) nextModes (currentAddress + 1) memory'
     in
        sequence $ getInstructionParams' (numberOfParams $ O.instruction opcode) (O.orderedParameterModes opcode) (address + 1) memory

translate :: O.ParameterMode -> Int -> InstructionParameter
translate O.Position = Position
translate O.Immediate = Immediate

nextInstructionPointer :: O.Instruction -> InstructionPointer -> InstructionPointer
nextInstructionPointer instruction (MkInstructionPointer address) =
    MkInstructionPointer (address + numberOfParams instruction + 1)

applyInstruction :: O.Instruction -> [Int] -> ApplicationState -> ApplicationState
applyInstruction O.Add inputs (MkState memory stdIn stdOut) = MkState (applyInstructionParams3 (+) inputs memory) stdIn stdOut
applyInstruction O.Multiply inputs (MkState memory stdIn stdOut) = MkState (applyInstructionParams3 (*) inputs memory) stdIn stdOut
applyInstruction O.Halt _ state = state
applyInstruction O.Read inputs state = readFromStdIn (head inputs) state
applyInstruction O.Write inputs state = writeToStdOut (head inputs) state

resolveParameters :: [InstructionParameter] -> AddressableMemory -> Maybe [Int]
resolveParameters params memory = mapM (resolveParameter memory) params

readFromStdIn :: AL.Address -> ApplicationState -> ApplicationState
readFromStdIn outputAddress (MkState memory stdIn stdOut) =
    let
        (maybeReadValue, resultingStdIn) = Day5.read stdIn
        updatedMemory = maybe memory (\readValue -> AL.replace outputAddress readValue memory) maybeReadValue
     in
        MkState updatedMemory resultingStdIn stdOut

writeToStdOut :: Int -> ApplicationState -> ApplicationState
writeToStdOut value (MkState memory stdIn stdOut) = MkState memory stdIn (write value stdOut)

resolveParameter :: AddressableMemory -> InstructionParameter -> Maybe Int
resolveParameter _ (Day5.Immediate value) = Just value
resolveParameter memory (Day5.Position address) = lookup address memory

applyInstructionParams3 :: (Int -> Int -> Int) -> [Int] -> AL.AddressableList Int -> AL.AddressableList Int
applyInstructionParams3 f inputs list =
    let input1 = head inputs
        input2 = inputs !! 1
        outputAddress = inputs !! 2
        computedValue = f input1 input2
        newMemory = AL.replace outputAddress computedValue list
     in trace (show (inputs, newMemory)) newMemory

runDay5 :: IO ()
runDay5 = do
    strings <- concatMap (splitOn ",") . lines <$> readFile "resources/day5.txt"
    let ints :: [Int] = map Prelude.read strings
    let memory = AL.create ints
    let testMemory = AL.create [1002, 4, 3, 4, 33]
    let stdIn = MkStdIn []
    let stdOut = MkStdOut []
    let endState = runProgram (12, 2) (MkState testMemory stdIn stdOut)
    print endState
    return ()
