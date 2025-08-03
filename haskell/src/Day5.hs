{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (runDay5) where

import qualified AddressableList as AL (Address, AddressableList, create, eitherLookup, replace)
import Data.List (uncons)
import Data.List.Split (splitOn)
import qualified Opcode as O (
    Instruction (Add, Halt, Multiply, Read, Write),
    Opcode,
    ParameterMode (Immediate, Position),
    defaultMode,
    instruction,
    orderedParameterModes,
    parseOpcodeValue,
 )
import Text.Printf (printf)

type ErrorMessage = String
type AddressableMemory = AL.AddressableList Int
data Param = Immediate Int | Position Int deriving (Show)
newtype InstructionPointer = MkInstructionPointer Int deriving (Show)

data InstructionAndParams
    = Add (Int, Int) AL.Address
    | Multiply (Int, Int) AL.Address
    | Halt
    | Read AL.Address
    | Write Int
    deriving (Show)

newtype StdIn = MkStdIn [Int] deriving (Show)
newtype StdOut = MkStdOut [Int] deriving (Show)
data ApplicationState = MkState AddressableMemory StdIn StdOut deriving (Show)

read :: StdIn -> (Maybe Int, StdIn)
read (MkStdIn []) = (Nothing, MkStdIn [])
read (MkStdIn (x : xs)) = (Just x, MkStdIn xs)

write :: Int -> StdOut -> StdOut
write x (MkStdOut xs) = MkStdOut (xs <> [x])

numberOfParams :: O.Instruction -> Int
numberOfParams O.Add = 3
numberOfParams O.Multiply = 3
numberOfParams O.Halt = 0
numberOfParams O.Read = 1
numberOfParams O.Write = 1

runProgram :: ApplicationState -> Either ErrorMessage ApplicationState
runProgram (MkState memory stdIn stdOut) = recurseProgram start (MkState memory stdIn stdOut)

recurseProgram :: InstructionPointer -> ApplicationState -> Either ErrorMessage ApplicationState
recurseProgram instructionPointer@(MkInstructionPointer instructionAddress) state@(MkState memory _ _) = do
    opcode <- O.parseOpcodeValue =<< AL.eitherLookup instructionAddress memory
    instructionAndParams <- getInstructionAndParams opcode instructionPointer memory
    case instructionAndParams of
        Halt -> Right state
        actionableInstruction -> recurseProgram (nextInstructionPointer (O.instruction opcode) instructionPointer) (applyInstruction actionableInstruction state)

start :: InstructionPointer
start = MkInstructionPointer 0

getInstructionAndParams :: O.Opcode -> InstructionPointer -> AddressableMemory -> Either ErrorMessage InstructionAndParams
getInstructionAndParams opcode instructionPointer memory = do
    parameters <- getInstructionParams opcode instructionPointer memory
    constructInstruction memory (O.instruction opcode) parameters

constructInstruction :: AddressableMemory -> O.Instruction -> [Param] -> Either ErrorMessage InstructionAndParams
constructInstruction memory O.Add [p1, p2, Position address] = do
    addend1 <- resolveParameter memory p1
    addend2 <- resolveParameter memory p2
    return (Add (addend1, addend2) address)
constructInstruction memory O.Multiply [p1, p2, Position address] = do
    multiplicand <- resolveParameter memory p1
    multiplier <- resolveParameter memory p2
    return (Multiply (multiplicand, multiplier) address)
constructInstruction _ O.Halt [] = Right Halt
constructInstruction _ O.Read [Position address] = Right (Read address)
constructInstruction memory O.Write [p1] = Write <$> resolveParameter memory p1
constructInstruction _ instruction params = Left (printf "Unable to parse parameters %s for instruction %s")

getInstructionParams :: O.Opcode -> InstructionPointer -> AddressableMemory -> Either ErrorMessage [Param]
getInstructionParams opcode (MkInstructionPointer address) memory =
    let
        getInstructionParams' :: Int -> [O.ParameterMode] -> AL.Address -> AddressableMemory -> [Either ErrorMessage Param]
        getInstructionParams' 0 _ _ _ = []
        getInstructionParams' count modes currentAddress memory' =
            let maybeHeadAndTail = uncons modes
                currentMode = maybe O.defaultMode fst maybeHeadAndTail
                nextModes = maybe [] snd maybeHeadAndTail
             in fmap (translate currentMode) (AL.eitherLookup currentAddress memory') : getInstructionParams' (count - 1) nextModes (currentAddress + 1) memory'
     in
        sequence $ getInstructionParams' (numberOfParams $ O.instruction opcode) (O.orderedParameterModes opcode) (address + 1) memory

translate :: O.ParameterMode -> Int -> Param
translate O.Position = Position
translate O.Immediate = Immediate

nextInstructionPointer :: O.Instruction -> InstructionPointer -> InstructionPointer
nextInstructionPointer instruction (MkInstructionPointer address) =
    MkInstructionPointer (address + numberOfParams instruction + 1)

applyInstruction :: InstructionAndParams -> ApplicationState -> ApplicationState
applyInstruction (Add (addend1, addend2) outputAddress) (MkState memory stdIn stdOut) = MkState (AL.replace outputAddress (addend1 + addend2) memory) stdIn stdOut
applyInstruction (Multiply (multiplicand, multipler) outputAddress) (MkState memory stdIn stdOut) = MkState (AL.replace outputAddress (multiplicand * multipler) memory) stdIn stdOut
applyInstruction Halt state = state
applyInstruction (Read address) state = readFromStdIn address state
applyInstruction (Write value) (MkState memory stdIn stdOut) = MkState memory stdIn (write value stdOut)

readFromStdIn :: AL.Address -> ApplicationState -> ApplicationState
readFromStdIn outputAddress (MkState memory stdIn stdOut) =
    let
        (maybeReadValue, resultingStdIn) = Day5.read stdIn
        updatedMemory = maybe memory (\readValue -> AL.replace outputAddress readValue memory) maybeReadValue
     in
        MkState updatedMemory resultingStdIn stdOut

resolveParameter :: AddressableMemory -> Param -> Either ErrorMessage Int
resolveParameter _ (Day5.Immediate value) = Right value
resolveParameter memory (Day5.Position address) = AL.eitherLookup address memory

runDay5 :: IO ()
runDay5 = do
    strings <- concatMap (splitOn ",") . lines <$> readFile "resources/day5.txt"
    let ints :: [Int] = map Prelude.read strings
    let memory = AL.create ints
    let stdIn = MkStdIn [1]
    let stdOut = MkStdOut []
    let endState = runProgram (MkState memory stdIn stdOut)
    print endState
    return ()
