{-# LANGUAGE ScopedTypeVariables #-}

module Day5 (runDay5) where

import qualified AddressableList as AL (Address, AddressableList, create, eitherLookup, replace)
import Control.Monad.Error.Class (liftEither)
import Control.Monad.State (MonadState (get), StateT, put, runStateT)
import Data.List (uncons)
import Data.List.Split (splitOn)
import qualified Data.Map as M
import Debug.Trace
import qualified Opcode as O (
    Opcode,
    ParameterMode (Immediate, Position),
    defaultMode,
    instructionCode,
    orderedParameterModes,
    parseOpcodeValue,
 )
import Text.Printf (printf)

type ErrorMessage = String
type AddressableMemory = AL.AddressableList Int
data Param = Immediate Int | Position Int deriving (Show)
newtype InstructionPointer = MkInstructionPointer Int deriving (Show)

data Instruction
    = Add Param Param Param
    | Multiply Param Param Param
    | Halt
    | Read Param
    | Write Param
    | JumpIfTrue Param Param
    | JumpIfFalse Param
    | LessThan Param Param Param
    | Equals Param Param Param
    deriving (Show)

data InstructionSpec = InstructionSpec
    { instructionName :: String
    , parameterCount :: Int
    , builder :: [Param] -> Either ErrorMessage Instruction
    }

instructionTable :: M.Map Int InstructionSpec
instructionTable =
    M.fromList
        [ (1, InstructionSpec "Add" 3 buildAdd)
        ]

type InstructionAndSpec = (Instruction, InstructionSpec)

buildAdd :: [Param] -> Either ErrorMessage Instruction
buildAdd [p1, p2, p3@(Position _)] = Right (Add p1 p2 p3)
buildAdd xs = Left (printf "Unable to create Add instructions from parameters %s" (show xs))

newtype StdIn = MkStdIn [Int] deriving (Show)
newtype StdOut = MkStdOut [Int] deriving (Show)
data ApplicationState = MkState InstructionPointer AddressableMemory StdIn StdOut deriving (Show)

type AppM = StateT ApplicationState (Either ErrorMessage) StdOut

read :: StdIn -> (Maybe Int, StdIn)
read (MkStdIn []) = (Nothing, MkStdIn [])
read (MkStdIn (x : xs)) = (Just x, MkStdIn xs)

write :: Int -> StdOut -> StdOut
write x (MkStdOut xs) = MkStdOut (xs <> [x])

runProgram :: AppM
runProgram = do
    (MkState instructionPointer memory _ stdOut) <- get
    opcode <- trace (show instructionPointer) liftEither (getOpcode instructionPointer memory)
    instructionAndSpec <- liftEither (getInstructionAndSpec opcode instructionPointer memory)
    case instructionAndSpec of
        (Halt, _) -> return stdOut
        actionableInstruction -> applyInstruction actionableInstruction >> runProgram

start :: InstructionPointer
start = MkInstructionPointer 0

getOpcode :: InstructionPointer -> AddressableMemory -> Either ErrorMessage O.Opcode
getOpcode (MkInstructionPointer address) memory = AL.eitherLookup address memory >>= O.parseOpcodeValue

getInstructionAndSpec :: O.Opcode -> InstructionPointer -> AddressableMemory -> Either ErrorMessage InstructionAndSpec
getInstructionAndSpec opcode instructionPointer memory = do
    instructionSpec <-
        maybe
            (Left $ printf "Encountered invalid instruction code %i" (O.instructionCode opcode))
            Right
            (M.lookup (O.instructionCode opcode) instructionTable)
    parameters <- getInstructionParams instructionSpec (O.orderedParameterModes opcode) instructionPointer memory
    instruction <- builder instructionSpec parameters
    Right (instruction, instructionSpec)

getInstructionParams :: InstructionSpec -> [O.ParameterMode] -> InstructionPointer -> AddressableMemory -> Either ErrorMessage [Param]
getInstructionParams instructionSpec parameterModes (MkInstructionPointer address) memory =
    let
        getInstructionParams' :: Int -> [O.ParameterMode] -> AL.Address -> AddressableMemory -> [Either ErrorMessage Param]
        getInstructionParams' 0 _ _ _ = []
        getInstructionParams' count modes currentAddress memory' =
            let maybeHeadAndTail = uncons modes
                currentMode = maybe O.defaultMode fst maybeHeadAndTail
                nextModes = maybe [] snd maybeHeadAndTail
             in fmap (translate currentMode) (AL.eitherLookup currentAddress memory') : getInstructionParams' (count - 1) nextModes (currentAddress + 1) memory'
     in
        sequence $ getInstructionParams' (parameterCount instructionSpec) parameterModes (address + 1) memory

translate :: O.ParameterMode -> Int -> Param
translate O.Position = Position
translate O.Immediate = Immediate

nextInstructionPointer :: InstructionSpec -> InstructionPointer -> InstructionPointer
nextInstructionPointer instructionSpec (MkInstructionPointer address) =
    MkInstructionPointer (address + parameterCount instructionSpec + 1)

applyInstruction :: InstructionAndSpec -> AppM
applyInstruction (Add param1 param2 (Position outputAddress), instructionSpec) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    addend1 <- liftEither $ resolveParameter memory param1
    addend2 <- liftEither $ resolveParameter memory param2
    put (MkState (nextInstructionPointer instructionSpec instructionPointer) (AL.replace outputAddress (addend1 + addend2) memory) stdIn stdOut)
    return stdOut
applyInstruction _ = liftEither (Left "Error!")

resolveParameter :: AddressableMemory -> Param -> Either ErrorMessage Int
resolveParameter _ (Day5.Immediate value) = Right value
resolveParameter memory (Day5.Position address) = AL.eitherLookup address memory

runDay5 :: IO ()
runDay5 = do
    strings <- concatMap (splitOn ",") . lines <$> readFile "resources/day5.txt"
    let ints :: [Int] = map Prelude.read strings
    let memory = AL.create ints
    let stdIn = MkStdIn [5]
    let stdOut = MkStdOut []
    let initialState = MkState start memory stdIn stdOut
    let result = runStateT runProgram initialState
    print result
    return ()
