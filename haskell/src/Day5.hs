{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (runDay5) where

import qualified AddressableList as AL (Address, AddressableList, create, eitherLookup, replace)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.State (MonadState (get), StateT, put, runStateT)
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
data ApplicationState = MkState InstructionPointer AddressableMemory StdIn StdOut deriving (Show)

type AppM = StateT ApplicationState (Either ErrorMessage) StdOut

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

runProgram :: AppM
runProgram = do
    (MkState instructionPointer memory _ stdOut) <- get
    opcode <- liftEither (getOpcode instructionPointer memory)
    instructionAndParams <- liftEither (getInstructionAndParams opcode instructionPointer memory)
    case instructionAndParams of
        Halt -> return stdOut
        actionableInstruction -> applyInstruction actionableInstruction >> runProgram

start :: InstructionPointer
start = MkInstructionPointer 0

getOpcode :: InstructionPointer -> AddressableMemory -> Either ErrorMessage O.Opcode
getOpcode (MkInstructionPointer address) memory = AL.eitherLookup address memory >>= O.parseOpcodeValue

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
constructInstruction _ instruction params = Left (printf "Unable to parse parameters %s for instruction %s" (show params) (show instruction))

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

applyInstruction :: InstructionAndParams -> AppM
applyInstruction (Add (addend1, addend2) outputAddress) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    put (MkState (nextInstructionPointer O.Add instructionPointer) (AL.replace outputAddress (addend1 + addend2) memory) stdIn stdOut)
    return stdOut
applyInstruction (Multiply (multiplicand, multipler) outputAddress) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    put (MkState (nextInstructionPointer O.Multiply instructionPointer) (AL.replace outputAddress (multiplicand * multipler) memory) stdIn stdOut)
    return stdOut
applyInstruction Halt = do
    (MkState _ _ _ stdOut) <- get
    return stdOut
applyInstruction (Read address) = readFromStdIn address
applyInstruction (Write value) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    let newStdOut = write value stdOut
    put (MkState (nextInstructionPointer O.Write instructionPointer) memory stdIn newStdOut)
    return newStdOut

readFromStdIn :: AL.Address -> AppM
readFromStdIn outputAddress = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    let (maybeReadValue, resultingStdIn) = Day5.read stdIn
    let updatedMemory = maybe memory (\readValue -> AL.replace outputAddress readValue memory) maybeReadValue
    put (MkState (nextInstructionPointer O.Read instructionPointer) updatedMemory resultingStdIn stdOut)
    return stdOut

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
    let initialState = MkState start memory stdIn stdOut
    let result = runStateT runProgram initialState
    print result
    return ()
