module Whacked.Backend.ARM.ASM where


import Data.List
import Whacked.Types



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


data ARMImm
  = ARMR ARMReg
  | ARMI Int
  deriving ( Eq, Ord )


instance Show ARMImm where
  show (ARMR reg)
    = show reg
  show (ARMI int)
    = "#" ++ show int


data ARMCond
  = ALT
  | AGT
  | ALTE
  | AGTE
  | AEQ
  | ANE
  | AAL
  deriving (Eq, Ord)


toARMCond :: CondOp -> ARMCond
toARMCond CLT  = ALT
toARMCond CLTE = ALTE
toARMCond CGT  = AGT
toARMCond CGTE = AGTE
toARMCond CEQ  = AEQ
toARMCond CNE  = ANE


instance Show ARMCond where
  show AAL  = ""
  show AGT  = "GT"
  show ALT  = "LT"
  show AGTE = "GT"
  show ALTE = "LT"
  show AEQ  = "EQ"
  show ANE  = "NE"


data ASM
  = ARMLabel Int
  | ARMFunc String
  | ARMSection String
  | ARMWord Int
  | ARMAscii String
  | ARMADR ARMReg String
  | ARMADD ARMReg ARMReg ARMImm
  | ARMSUB ARMReg ARMReg ARMImm
  | ARMCMP ARMReg ARMImm
  | ARMMOV ARMReg ARMImm
  | ARMPush [ARMReg]
  | ARMPop [ARMReg]
  | ARMLDR ARMReg Int
  | ARMSTR ARMReg ARMReg ARMReg
  | ARMBL String
  | ARMB ARMCond Int
  deriving (Eq, Ord)


instance Show ASM where
  show (ARMSection section)
    = ".section " ++ section
  show (ARMLabel label)
    = "L" ++ show label ++ ":"
  show (ARMFunc label)
    = ".global " ++ label ++ "\n" ++ label ++ ":"
  show (ARMWord x)
    = "\t.word " ++ show x
  show (ARMAscii x)
    = "\t.ascii " ++ show x

  show (ARMADR d label)
    = "\tLDR " ++ show d ++ ", =" ++ label
  show (ARMADD d n m)
    = "\tADD " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMSUB d n m)
    = "\tSUB " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMMOV d n)
    = "\tMOV " ++ show d ++ ", " ++ show n
  show (ARMCMP n m)
    = "\tCMP " ++ show n ++ ", " ++ show m

  show (ARMPush rs)
    = "\tPUSH {" ++ concat (intersperse ", " (map show rs)) ++ "}"
  show (ARMPop rs)
    = "\tPOP {" ++ concat (intersperse ", " (map show rs)) ++ "}"
  show (ARMLDR d n)
    = "\tLDR " ++ show d ++ ", =" ++ show n
  show (ARMSTR d n m)
    = "\tST " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMB cond xs)
    = "\tB" ++ show cond ++ " L" ++ show xs
  show (ARMBL xs)
    = "\tBL " ++ show xs
