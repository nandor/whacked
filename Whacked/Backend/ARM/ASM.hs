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
  | ALTE
  | AGTE
  | AEQ
  | ANE
  | AAL
  deriving (Eq, Ord)


instance Show ARMCond where
  show AAL  = ""
  show AGT  = "GT"
  show ALT  = "LT"
  show AGTE = "GT"
  show ALTE = "LT"
  show AEQ  = "EQ"
  show ANE  = "NE"


data ASM
  = ARMLabel String
  | ARMGlobal String
  | ARMSection String
  | ARMWord Int
  | ARMAscii String
  | ARMADR ARMReg String
  | ARMAdd ARMReg ARMReg ARMReg
  | ARMPush [ARMReg]
  | ARMPop [ARMReg]
  | ARMMov ARMReg ARMReg
  | ARMLDR ARMReg Int
  | ARMSTR ARMReg ARMReg ARMReg
  | ARMBL String
  | ARMB ARMCond String
  | ARMCmp ARMReg ARMReg
  deriving (Eq, Ord)


instance Show ASM where
  show (ARMSection section)
    = ".section " ++ section
  show (ARMGlobal label)
    = ".global " ++ label
  show (ARMLabel label)
    = "" ++ label ++ ":"
  show (ARMWord x)
    = "\t.word " ++ show x
  show (ARMAscii x)
    = "\t.ascii " ++ show x
  show (ARMADR d label)
    = "\tLDR " ++ show d ++ ", =" ++ label
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
