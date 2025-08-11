{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

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

newtype InstructionPointer = MkInstructionPointer Int deriving (Show)

data Immediate
data Position

data Param mode where
    Imm :: Int -> Param Immediate
    Pos :: Int -> Param Position

deriving instance Show (Param mode)

data AnyParam where
    AnyParam :: Param mode -> AnyParam

deriving instance Show AnyParam

data Instruction where
    Add :: AnyParam -> AnyParam -> Param Position -> Instruction
    Multiply :: AnyParam -> AnyParam -> Param Position -> Instruction
    Halt :: Instruction
    Read :: Param Position -> Instruction
    Write :: AnyParam -> Instruction
    JumpIfTrue :: AnyParam -> AnyParam -> Instruction
    JumpIfFalse :: AnyParam -> AnyParam -> Instruction
    LessThan :: AnyParam -> AnyParam -> Param Position -> Instruction
    Equals :: AnyParam -> AnyParam -> Param Position -> Instruction

deriving instance Show Instruction

data InstructionSpec = InstructionSpec
    { instructionName :: String
    , parameterCount :: Int
    , builder :: InstructionSpec -> [AnyParam] -> Either ErrorMessage Instruction
    }

instructionTable :: M.Map Int InstructionSpec
instructionTable =
    M.fromList
        [ (1, InstructionSpec "Add" 3 (buildAnyAnyPos Add))
        , (2, InstructionSpec "Multiply" 3 (buildAnyAnyPos Multiply))
        , (3, InstructionSpec "Read" 1 (buildPos Read))
        , (4, InstructionSpec "Write" 1 (buildAny Write))
        , (5, InstructionSpec "Jump-If-True" 2 (buildAnyAny JumpIfTrue))
        , (6, InstructionSpec "Jump-If-False" 2 (buildAnyAny JumpIfFalse))
        , (7, InstructionSpec "LessThan" 3 (buildAnyAnyPos LessThan))
        , (8, InstructionSpec "Equals" 3 (buildAnyAnyPos Equals))
        , (99, InstructionSpec "Halt" 0 (buildEmpty Halt))
        ]

buildEmpty :: Instruction -> InstructionSpec -> [AnyParam] -> Either ErrorMessage Instruction
buildEmpty f _ [] = Right f
buildEmpty _ spec params = Left (buildError spec params)

buildAnyAnyPos :: (AnyParam -> AnyParam -> Param Position -> Instruction) -> InstructionSpec -> [AnyParam] -> Either ErrorMessage Instruction
buildAnyAnyPos f _ [anyParam1, anyParam2, AnyParam position@(Pos _)] = Right (f anyParam1 anyParam2 position)
buildAnyAnyPos _ spec params = Left (buildError spec params)

buildPos :: (Param Position -> Instruction) -> InstructionSpec -> [AnyParam] -> Either ErrorMessage Instruction
buildPos f _ [AnyParam position@(Pos _)] = Right (f position)
buildPos _ spec params = Left (buildError spec params)

buildAny :: (AnyParam -> Instruction) -> InstructionSpec -> [AnyParam] -> Either ErrorMessage Instruction
buildAny f _ [anyParam] = Right (f anyParam)
buildAny _ spec params = Left (buildError spec params)

buildAnyAny :: (AnyParam -> AnyParam -> Instruction) -> InstructionSpec -> [AnyParam] -> Either ErrorMessage Instruction
buildAnyAny f _ [anyParam1, anyParam2] = Right (f anyParam1 anyParam2)
buildAnyAny _ spec params = Left (buildError spec params)

type InstructionAndSpec = (Instruction, InstructionSpec)

buildError :: InstructionSpec -> [AnyParam] -> String
buildError instructionSpec params = printf "Unable to create %s instruction from parameters %s" (instructionName instructionSpec) (show params)

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
    instruction <- builder instructionSpec instructionSpec parameters
    Right (instruction, instructionSpec)

getInstructionParams :: InstructionSpec -> [O.ParameterMode] -> InstructionPointer -> AddressableMemory -> Either ErrorMessage [AnyParam]
getInstructionParams instructionSpec parameterModes (MkInstructionPointer address) memory =
    let
        translate :: O.ParameterMode -> Int -> AnyParam
        translate O.Immediate = AnyParam . Imm
        translate O.Position = AnyParam . Pos

        getInstructionParams' :: Int -> [O.ParameterMode] -> AL.Address -> AddressableMemory -> [Either ErrorMessage AnyParam]
        getInstructionParams' 0 _ _ _ = []
        getInstructionParams' count modes currentAddress memory' =
            let maybeHeadAndTail = uncons modes
                currentMode = maybe O.defaultMode fst maybeHeadAndTail
                nextModes = maybe [] snd maybeHeadAndTail
             in fmap (translate currentMode) (AL.eitherLookup currentAddress memory') : getInstructionParams' (count - 1) nextModes (currentAddress + 1) memory'
     in
        sequence $ getInstructionParams' (parameterCount instructionSpec) parameterModes (address + 1) memory

nextInstructionPointer :: InstructionSpec -> InstructionPointer -> InstructionPointer
nextInstructionPointer instructionSpec (MkInstructionPointer address) =
    MkInstructionPointer (address + parameterCount instructionSpec + 1)

applyInstruction :: InstructionAndSpec -> AppM
applyInstruction (Add (AnyParam param1) (AnyParam param2) (Pos outputAddress), instructionSpec) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    addend1 <- liftEither $ resolveParameter memory param1
    addend2 <- liftEither $ resolveParameter memory param2
    put (MkState (nextInstructionPointer instructionSpec instructionPointer) (AL.replace outputAddress (addend1 + addend2) memory) stdIn stdOut)
    return stdOut
applyInstruction (Multiply (AnyParam param1) (AnyParam param2) (Pos outputAddress), instructionSpec) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    multiplicand1 <- liftEither $ resolveParameter memory param1
    multiplicand2 <- liftEither $ resolveParameter memory param2
    put (MkState (nextInstructionPointer instructionSpec instructionPointer) (AL.replace outputAddress (multiplicand1 * multiplicand2) memory) stdIn stdOut)
    return stdOut
applyInstruction (Halt, _) = do
    liftEither (Left "Attempting to apply Halt instruction")
applyInstruction (Read (Pos outputAddress), instructionSpec) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    let (maybeReadValue, resultingStdIn) = Day5.read stdIn
    let updatedMemory = maybe memory (\readValue -> AL.replace outputAddress readValue memory) maybeReadValue
    put (MkState (nextInstructionPointer instructionSpec instructionPointer) updatedMemory resultingStdIn stdOut)
    return stdOut
applyInstruction (Write (AnyParam parameter), instructionSpec) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    valueToWrite <- liftEither (resolveParameter memory parameter)
    let resultingStdOut = write valueToWrite stdOut
    put (MkState (nextInstructionPointer instructionSpec instructionPointer) memory stdIn resultingStdOut)
    return resultingStdOut
applyInstruction (JumpIfTrue (AnyParam param1) (AnyParam param2), instructionSpec) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    valueToTest <- liftEither (resolveParameter memory param1)
    conditionalNextInstructionPointer <- liftEither (resolveParameter memory param2)
    let nextInstructionPointer' =
            if valueToTest /= 0
                then MkInstructionPointer conditionalNextInstructionPointer
                else nextInstructionPointer instructionSpec instructionPointer
    put (MkState nextInstructionPointer' memory stdIn stdOut)
    return stdOut
applyInstruction (JumpIfFalse (AnyParam param1) (AnyParam param2), instructionSpec) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    valueToTest <- liftEither (resolveParameter memory param1)
    conditionalNextInstructionPointer <- liftEither (resolveParameter memory param2)
    let nextInstructionPointer' =
            if valueToTest == 0
                then MkInstructionPointer conditionalNextInstructionPointer
                else nextInstructionPointer instructionSpec instructionPointer
    put (MkState nextInstructionPointer' memory stdIn stdOut)
    return stdOut
applyInstruction (LessThan (AnyParam param1) (AnyParam param2) (Pos outputAddress), instructionSpec) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    value1 <- liftEither (resolveParameter memory param1)
    value2 <- liftEither (resolveParameter memory param2)
    let valueToWrite = if value1 < value2 then 1 :: Int else 0
    put (MkState (nextInstructionPointer instructionSpec instructionPointer) (AL.replace outputAddress valueToWrite memory) stdIn stdOut)
    return stdOut
applyInstruction (Equals (AnyParam param1) (AnyParam param2) (Pos outputAddress), instructionSpec) = do
    (MkState instructionPointer memory stdIn stdOut) <- get
    value1 <- liftEither (resolveParameter memory param1)
    value2 <- liftEither (resolveParameter memory param2)
    let valueToWrite = if value1 == value2 then 1 :: Int else 0
    put (MkState (nextInstructionPointer instructionSpec instructionPointer) (AL.replace outputAddress valueToWrite memory) stdIn stdOut)
    return stdOut

resolveParameter :: AddressableMemory -> Param mode -> Either ErrorMessage Int
resolveParameter _ (Imm value) = Right value
resolveParameter memory (Pos address) = AL.eitherLookup address memory

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
