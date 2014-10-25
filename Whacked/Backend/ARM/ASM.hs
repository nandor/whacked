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
  | R12
  | SP
  | LR
  | PC
  deriving ( Eq, Ord, Show, Enum )


data ARMLoc
  = ARMLocReg ARMReg
  | ARMLocStk Int
  | ARMArgIn Int
  deriving ( Eq, Ord )


argInLocation :: [ARMLoc]
argInLocation
  = [ARMLocReg x | x <- enumFromTo R0 R3 ] ++ [ ARMArgIn x | x <- [1..] ]


instance Show ARMLoc where
  show (ARMLocReg x)
    = show x
  show (ARMLocStk x)
    = "%s" ++ show x
  show (ARMArgIn x)
    = "%i" ++ show x


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
  show AGTE = "GTE"
  show ALTE = "LTE"
  show AEQ  = "EQ"
  show ANE  = "NE"


data ASM
  = ARMLabel Int
  | ARMFunc String
  | ARMSection String
  | ARMWord Int
  | ARMAscii String


  | ARMLoadConst ARMReg Int
  | ARMLoadMem   ARMReg ARMReg Int
  | ARMStoreMem  ARMReg ARMReg Int
  | ARMMov       ARMReg ARMImm


  | ARMADR ARMReg String
  | ARMAdd ARMReg ARMReg ARMImm
  | ARMSub ARMReg ARMReg ARMImm
  | ARMCmp ARMReg ARMImm
  | ARMPUSH [ARMReg]
  | ARMPOP  [ARMReg]

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

  show (ARMLoadConst d const)
    = "\tLDR " ++ show d ++ ", =" ++ show const
  show (ARMLoadMem d base off)
    = "\tLDR " ++ show d ++ ", [" ++ show base ++ ", #" ++ show off ++ "]"
  show (ARMStoreMem d base off)
    = "\tSTR " ++ show d ++ ", [" ++ show base ++ ", #" ++ show off ++ "]"
  show (ARMMov d n)
    = "\tMOV " ++ show d ++ ", " ++ show n


  show (ARMAdd d n m)
    = "\tADD " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMSub d n m)
    = "\tSUB " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMCmp n m)
    = "\tCMP " ++ show n ++ ", " ++ show m

  show (ARMPUSH rs)
    = "\tPUSH {" ++ concat (intersperse ", " (map show rs)) ++ "}"
  show (ARMPOP rs)
    = "\tPOP {" ++ concat (intersperse ", " (map show rs)) ++ "}"
  show (ARMSTR d n m)
    = "\tST " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMB cond xs)
    = "\tB" ++ show cond ++ " L" ++ show xs
  show (ARMBL xs)
    = "\tBL " ++ xs
