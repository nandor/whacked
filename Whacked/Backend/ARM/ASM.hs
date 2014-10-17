module Whacked.Backend.ARM.ASM where


import Data.List



data ARMReg
  = R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | BP
  | SP
  | LR
  | PC
  deriving (Eq, Ord, Show, Enum)


data ARMCond
  = ALT
  | AGT
  | AAL
  deriving (Eq, Ord)


instance Show ARMCond where
  show AAL = ""
  show AGT = "GT"
  show ALT = "LT"


data ASM
  = ARMLabel String
  | ARMAdd ARMReg ARMReg ARMReg
  | ARMPush [ARMReg]
  | ARMPop [ARMReg]
  | ARMMov ARMReg ARMReg
  | ARMLDR ARMReg Int
  | ARMBL String
  | ARMB ARMCond String
  | ARMCmp ARMReg ARMReg
  deriving (Eq, Ord)


instance Show ASM where
  show (ARMLabel label)
    = "" ++ label ++ ":"
  show (ARMAdd d n m)
    = "\tADD " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMPush rs)
    = "\tPUSH {" ++ concat (intersperse ", " (map show rs)) ++ "}"
  show (ARMPop rs)
    = "\tPOP {" ++ concat (intersperse ", " (map show rs)) ++ "}"
  show (ARMMov d n)
    = "\tMOV " ++ show d ++ ", " ++ show n
  show (ARMLDR d n)
    = "\tLDR " ++ show d ++ ", =" ++ show n
  show (ARMB cond xs)
    = "\tB" ++ show cond ++ " " ++ xs
  show (ARMBL xs)
    = "\tBL " ++ xs
  show (ARMCmp n m)
    = "\tCMP " ++ show n ++ ", " ++ show m
