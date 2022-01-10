-- Turing Machine Simulator; Jessica Chavez for CSCI131: PLs
-- Dec. 15, 2021.

import Data.Map
import qualified System.Environment    as SysEnv
import qualified System.FilePath as FilePath

-- example Turing machines:
statesExample1 =
      [
      (StateNum 0,
      [Instruction {readSymbol = Zero, writeSymbol = Zero,
            move = TMLeft, goto = Halt},
      Instruction {readSymbol = One, writeSymbol = One,
            move = TMRight, goto = Halt},
      Instruction {readSymbol = Blank, writeSymbol = Blank,
            move = TMLeft, goto = StateNum 1}]), 
      (StateNum 1,
      [Instruction {readSymbol = Zero, writeSymbol = One,
            move = TMRight, goto = Halt},
      Instruction {readSymbol = One, writeSymbol = One,
            move = TMLeft, goto = StateNum 1},
      Instruction {readSymbol = Blank, writeSymbol = One,
            move = TMRight, goto = Halt}])
      ]

statesExample2 =
      [
      (StateNum 0,
      [Instruction {readSymbol = Zero, writeSymbol = Zero,
            move = TMLeft, goto = Halt},
      Instruction {readSymbol = One, writeSymbol = One,
            move = TMRight, goto = Halt},
      Instruction {readSymbol = Blank, writeSymbol = Blank,
            move = TMLeft, goto = StateNum 1}]), 
      (StateNum 1,
      [Instruction {readSymbol = Zero, writeSymbol = One,
            move = TMRight, goto = StateNum 2},
      Instruction {readSymbol = One, writeSymbol = One,
            move = TMLeft, goto = StateNum 1},
      Instruction {readSymbol = Blank, writeSymbol = One,
            move = TMRight, goto = Halt}]),
      (StateNum 2,
      [Instruction {readSymbol = Zero, writeSymbol = Zero,
            move = TMLeft, goto = Halt},
      Instruction {readSymbol = One, writeSymbol = One,
            move = TMRight, goto = StateNum 2},
      Instruction {readSymbol = Blank, writeSymbol = Blank,
            move = TMLeft, goto = StateNum 3}]),
      (StateNum 3,
      [Instruction {readSymbol = Zero, writeSymbol = Zero,
            move = TMLeft, goto = Halt},
      Instruction {readSymbol = One, writeSymbol = Blank,
            move = TMRight, goto = StateNum 3},
      Instruction {readSymbol = Blank, writeSymbol = Blank,
            move = TMLeft, goto = Halt}])
      ]

statesExample3 = 
      [
      (StateNum 0,
      [Instruction {readSymbol = Zero, writeSymbol = Zero,
            move = TMLeft, goto = Halt},
      Instruction {readSymbol = One, writeSymbol = One,
            move = TMLeft, goto = Halt},
      Instruction {readSymbol = Blank, writeSymbol = Blank,
            move = TMLeft, goto = StateNum 1}]),
      (StateNum 1,
      [Instruction {readSymbol = Zero, writeSymbol = One,
            move = TMLeft, goto = StateNum 1},
      Instruction {readSymbol = One, writeSymbol = Zero,
            move = TMLeft, goto = StateNum 1},
      Instruction {readSymbol = Blank, writeSymbol = Blank,
            move = TMRight, goto = StateNum 0}])
      ]
      

-- The different symbols on the Tape should be Zero = "0", One = "1", and Blank = "_"
data Symbol = Zero
             | One
             | Blank
    deriving (Show, Eq)  -- We want to be able to print these

-- You should be able to move left and right on the Tape!
data Move = TMRight
          | TMLeft
    deriving (Show, Eq)


-- the head of the Tape is just a Symbol
-- tapes should be (left of head) (head of Tape) (right of head)
      -- where "left of head" and "right of Tape" are lists of Symbols, but
            -- "left of head" is reversed.

      -- for example, for a tape like _ _ _ 1 0 1 0 [0] 1 _ _ _,
            -- left Tape = [Zero, One, Zero, One, Blank, Blank, Blank] (right-to-left)
            -- head = Zero
            -- right Tape = [One, Blank, Blank, Blank] (in left-to-right order)
type Tape = ([Symbol], Symbol, [Symbol])

-- We can GoTo a couple of different things:
      -- Halt (stay in the current State)
      -- StateNum: the numerical label of some State
data State = Halt
           | StateNum Int
          deriving(Ord, Show, Eq)


-- Make a data structure to store information about a State's response to each Symbol
      -- readSymbol: the Symbol of the current head of the Tape
      -- writeSymbol: the Symbol we want to replace the current head's Symbol with
            -- while in the current State
      -- move: the direction we want to Move in next
      -- goto: the next State to transition to (could be Halt or some StateNum _)
data Instruction = Instruction {
                     readSymbol :: Symbol,
                     writeSymbol :: Symbol,
                     move :: Move,
                     goto :: State
                     } 
                  deriving(Show)

-- A TransitionLookup is a lookup table of Turing machine States and their instructions
      -- the list of Instructions is a 3-element list in a particular order:
            -- Instructions for if the current Symbol in the current state is:
                  -- a Zero
                  -- a One
                  -- a Blank
      -- Using this TransitionLookup, we can easily find the Instructions for each State
type TransitionLookup = Map State [Instruction]

{-
Following: example Turing machines for testing, using stateExamples 1 and 2
from the top of this file.
-}
tl1 :: TransitionLookup -- Turing machine with 2 States
tl1 = fromList statesExample1

-- from project description. "Merges two groups of 1's separated by a zero, when started 
      -- in at the first blank to the right of a group of 1's."
tl2 :: TransitionLookup -- Turing machine with 4 States
tl2 = fromList statesExample2

-- "inverts" tape (turns 0's to 1's and 1's to 0's) when started at the first blank
      -- to the left of either a 1 or 0
tl3 :: TransitionLookup -- Turing machine with 1 State
tl3 = fromList statesExample3

-- in test Tapes, the left side of the Tape is reversed
      -- this makes it easier to access the "rightmost" element of the left list
            -- i.e. the element closest to the head of the Tape
testTape1 :: Tape
testTape1 = (
      [One, One, One, One, One, Zero, One, One, One, Blank, Blank, Blank],
       Blank,
      [Blank, Blank, Blank])

testTape2 :: Tape
testTape2 = (
      [Zero, One, One, One, One, One, Zero, One, One, One, Blank, Blank, Blank],
       Blank,
      [Blank, Blank, Blank])

testTape3 :: Tape
testTape3 = (
      [One, Zero, Zero, Zero, One, Zero, One, Zero, One, Blank, Blank, Blank],
       One,
      [Blank, Blank, Blank])

--------------------------- End example TMs ---------------------------

-- getInstructionList is a helper function for getting Instructions for each possible
      -- Symbol for the current State from a TransitionLookup table
getInstructionList :: TransitionLookup -> State -> [Instruction]
getInstructionList transitions state = findWithDefault [] state transitions

-- getInstruction gets the particular Instruction for the current Symbol at the Tape head
getInstruction :: TransitionLookup -> State -> Tape -> Instruction
getInstruction transitions state (_, symbol, _) =
            if symbol == Zero
            then head instructions
            else if symbol == One
            then instructions!!1
            else instructions!!2
      where instructions = getInstructionList transitions state

-- getNextState is a helper function for getting the State we want to transition to next
      -- for a particular Tape head Symbol in our current State,
      -- using a TransitionLookup table
getNextState :: TransitionLookup -> Tape -> State -> State 
getNextState transitions tape state =
      goto (getInstruction transitions state tape)

-- Using the same technique as in getNextState, get the direction we should move next
      -- based on our current State and current Tape head
getNextMove :: TransitionLookup -> Tape -> State -> Move
getNextMove transitions tape state =
      move (getInstruction transitions state tape)

writeTape :: TransitionLookup -> Tape -> State -> Tape
writeTape transitions (lt, h, rt) state = 
      (lt, writeSymbol (getInstruction transitions state (lt, h, rt)), rt)

-- We also want to be able to move left and right on the Tape
--- moveOnTape is a function that takes a Move and a Tape and returns the Tape after
      -- completing the Move
moveOnTape :: Move -> Tape -> Tape
moveOnTape TMLeft (lt, h, rt) = (tail lt, head lt, h : rt)
moveOnTape TMRight (lt, h, rt) = (h : lt, head rt, tail rt)

-- In order to take a single step, we want to perform the next Move on the new
      -- (newly overwritten) Tape
oneStep :: TransitionLookup -> Tape -> State -> Tape
oneStep transitions tape state = 
      moveOnTape (getNextMove transitions tape state) (writeTape transitions tape state)

--
simulationTM :: TransitionLookup -> Tape -> State -> Tape
simulationTM transitions tape state =
      if state == Halt
      then tape
      else (simulationTM transitions newTape newState)
            where newTape = oneStep transitions tape state
                  newState = getNextState transitions tape state


{- PART #2: Visualization-}
{- write string to Dot function; iterate through each State in TransitionLookup,
      convert each state to a Dot instruction as a String -}
transitionLookupToDot :: TransitionLookup -> [String]
transitionLookupToDot transitions =
      ["digraph D {"] ++ ["init -> 0;"] ++ getDotInstructions transitions keys
            ++ ["Halt -> Halt;"] ++ ["}"]
            where keys = (getTupleKeys (toList transitions))


getDotInstructions :: TransitionLookup ->  [State] -> [String]
getDotInstructions transitions (state:states) =
      (instructionsToDot state insts) ++ (getDotInstructions transitions states)
       where insts = (getInstructionList transitions state)
getDotInstructions transitions [] =
      []


instructionsToDot :: State -> [Instruction] -> [String]
instructionsToDot (StateNum label) (i:is) =
      [(show label) ++ " -> " ++ (show (goto i)) ++ " [label = ' " ++ 
            (show (readSymbol i)) ++ ": (" ++ (show (writeSymbol i)) ++ ", " ++ 
            (show (move i)) ++ ")'];"] ++ (instructionsToDot (StateNum label) is)
instructionsToDot Halt _ = []  -- we really shouldn't be trying to look up "Halt".
instructionsToDot _ [] =
      []


-- helper function: create a list of keys from a TransitionLookup Map
getTupleKeys :: [(State, [Instruction])] -> [State]
getTupleKeys (t:ts) =
      [(fst t)] ++ (getTupleKeys ts)
getTupleKeys [] =
      []

--------------------------------------------------------------------------------
-- Helper function to create .dot files  -- based on code from HW5
--------------------------------------------------------------------------------
emitCode :: String -> [String] -> IO ()
emitCode dotFileName dotCode =
    do putStrLn ("Writing " ++ dotFileName)
       writeFile dotFileName (unlines dotCode)

-- gets file name for Dot file from command line, transfers TransitionLookup tl2 over
run :: String -> IO ()
run filename =
  do putStrLn ("Reading " ++ filename)
     let dotFileName = FilePath.replaceExtension filename "gv"
     emitCode dotFileName (transitionLookupToDot tl2)

main :: IO ()
main =
  do args <- SysEnv.getArgs
     let filename = case args of
                      [arg] -> arg
                      _ -> error "exactly one filename expected"
     run filename
