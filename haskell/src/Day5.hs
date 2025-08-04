{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (runDay5) where

import qualified AddressableList as AL (Address, AddressableList, create, eitherLookup, replace)
import Control.Monad.Identity
import Control.Monad.State (MonadState (get), State, put, runStateT)
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

type AppM = State ApplicationState (Either ErrorMessage StdOut)

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

numberOfParams'' :: InstructionAndParams -> Int
numberOfParams'' (Add _ _) = 3
numberOfParams'' (Multiply _ _) = 3
numberOfParams'' Halt = 0
numberOfParams'' (Read _) = 1
numberOfParams'' (Write _) = 1

runProgram :: AppM
runProgram = do recurseProgram start

recurseProgram :: InstructionPointer -> AppM
recurseProgram instructionPointer@(MkInstructionPointer instructionAddress) = do
    (MkState memory _ stdOut) <- get
    let eitherErrorMessageOrInstructionAndParams =
            AL.eitherLookup instructionAddress memory
                >>= O.parseOpcodeValue
                >>= (\opcode' -> getInstructionAndParams opcode' instructionPointer memory)
    either
        (return . Left)
        ( \instructionAndParams -> case instructionAndParams of
            Halt -> return (Right stdOut)
            actionableInstruction ->
                applyInstruction' actionableInstruction
                    >> recurseProgram (nextInstructionPointer actionableInstruction instructionPointer)
        )
        eitherErrorMessageOrInstructionAndParams

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

nextInstructionPointer :: InstructionAndParams -> InstructionPointer -> InstructionPointer
nextInstructionPointer instruction (MkInstructionPointer address) =
    MkInstructionPointer (address + numberOfParams'' instruction + 1)

applyInstruction' :: InstructionAndParams -> AppM
applyInstruction' (Add (addend1, addend2) outputAddress) = do
    (MkState memory stdIn stdOut) <- get
    put (MkState (AL.replace outputAddress (addend1 + addend2) memory) stdIn stdOut)
    return (Right stdOut)
applyInstruction' (Multiply (multiplicand, multipler) outputAddress) = do
    (MkState memory stdIn stdOut) <- get
    put (MkState (AL.replace outputAddress (multiplicand * multipler) memory) stdIn stdOut)
    return (Right stdOut)
applyInstruction' Halt = do
    (MkState _ _ stdOut) <- get
    return (Right stdOut)
applyInstruction' (Read address) = readFromStdIn address
applyInstruction' (Write value) = do
    (MkState memory stdIn stdOut) <- get
    let newStdOut = write value stdOut
    put (MkState memory stdIn newStdOut)
    return (Right newStdOut)

readFromStdIn :: AL.Address -> AppM
readFromStdIn outputAddress = do
    (MkState memory stdIn stdOut) <- get
    let (maybeReadValue, resultingStdIn) = Day5.read stdIn
    let updatedMemory = maybe memory (\readValue -> AL.replace outputAddress readValue memory) maybeReadValue
    put (MkState updatedMemory resultingStdIn stdOut)
    return (Right stdOut)

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
    let initialState = MkState memory stdIn stdOut
    let Identity (result, _) = runStateT runProgram initialState
    print result
    return ()
