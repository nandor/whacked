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
  = ARMLocReg    ARMReg
  | ARMLocStk    Int
  | ARMLocArgIn  Int
  | ARMLocArgOut Int
  deriving ( Eq, Ord )


argInLocation :: Int -> [ARMLoc]
argInLocation count
  = [ARMLocReg x | x <- enumFromTo R0 R3 ] ++
    [ARMLocArgIn x | x <- [(count - 5), (count - 6)..0] ]


argOutLocation :: Int -> [ARMLoc]
argOutLocation count
  = [ARMLocReg x | x <- enumFromTo R0 R3 ] ++
    [ARMLocArgOut x | x <- [0..count - 5] ]



instance Show ARMLoc where
  show (ARMLocReg x)
    = show x
  show (ARMLocStk x)
    = "%s" ++ show x
  show (ARMLocArgIn x)
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
  | ALE
  | AGE
  | AEQ
  | ANE
  | AAL
  deriving (Eq, Ord)


toARMCond :: CondOp -> ARMCond
toARMCond CLT = ALT
toARMCond CLE = ALE
toARMCond CGT = AGT
toARMCond CGE = AGE
toARMCond CEQ = AEQ
toARMCond CNE = ANE


instance Show ARMCond where
  show AAL  = ""
  show AGT  = "GT"
  show ALT  = "LT"
  show AGE  = "GE"
  show ALE  = "LE"
  show AEQ  = "EQ"
  show ANE  = "NE"


data ASM
  = ARMLabel Int
  | ARMFunc String
  | ARMSection String
  | ARMString String String
  | ARMLoadConst ARMReg Int
  | ARMLdr    ARMReg ARMReg ARMImm
  | ARMStr    ARMReg ARMReg ARMImm
  | ARMLdrb   ARMReg ARMReg ARMImm
  | ARMStrb   ARMReg ARMReg ARMImm
  | ARMLdrLsl ARMReg ARMReg ARMReg Int
  | ARMStrLsl ARMReg ARMReg ARMReg Int
  | ARMAdr    ARMReg String
  | ARMAdd    ARMCond ARMReg ARMReg ARMImm
  | ARMSub    ARMCond ARMReg ARMReg ARMImm
  | ARMOrr    ARMCond ARMReg ARMReg ARMImm
  | ARMAnd    ARMCond ARMReg ARMReg ARMImm
  | ARMCmp    ARMCond ARMReg ARMImm
  | ARMTst    ARMCond ARMReg ARMImm
  | ARMMov    ARMCond ARMReg ARMImm
  | ARMMvn    ARMCond ARMReg ARMImm
  | ARMNeg    ARMCond ARMReg ARMImm
  | ARMSmull  ARMCond ARMReg ARMReg ARMReg ARMReg
  | ARMB      ARMCond Int
  | ARMPUSH [ARMReg]
  | ARMPOP  [ARMReg]
  | ARMBL String
  deriving (Eq, Ord)


instance Show ASM where
  show (ARMSection section)
    = ".section " ++ section
  show (ARMLabel label)
    = "L" ++ show label ++ ":"
  show (ARMFunc label)
    = ".global " ++ label ++ "\n" ++ label ++ ":"
  show (ARMString label string)
    = ".long " ++ show (length string) ++ "\n" ++
      label ++ ":\n" ++
      ".ascii " ++ show string

  show (ARMLoadConst d const)
    = "\tLDR " ++ show d ++ ", =" ++ show const
  show (ARMLdr d base off)
    = "\tLDR " ++ show d ++ ", [" ++ show base ++ ", " ++ show off ++ "]"
  show (ARMStr d base off)
    = "\tSTR " ++ show d ++ ", [" ++ show base ++ ", " ++ show off ++ "]"
  show (ARMLdrb d base off)
    = "\tLDRB " ++ show d ++ ", [" ++ show base ++ ", " ++ show off ++ "]"
  show (ARMStrb d base off)
    = "\tSTRB " ++ show d ++ ", [" ++ show base ++ ", " ++ show off ++ "]"
  show (ARMAdr d label)
    = "\tLDR " ++ show d ++ ", =" ++ label
  show (ARMLdrLsl d base reg lsl)
    = "\tLDR " ++ show d ++ ", [" ++ show base ++ ", " ++
      show reg ++ ", lsl #" ++ show lsl ++ "]"
  show (ARMStrLsl d base reg lsl)
    = "\tSTR " ++ show d ++ ", [" ++ show base ++ ", " ++
      show reg ++ ", lsl #" ++ show lsl ++ "]"


  show (ARMAdd cond d n m)
    = "\tADD" ++ show cond ++ " " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMSub cond d n m)
    = "\tSUB" ++ show cond ++ " " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMOrr cond d n m)
    = "\tORR" ++ show cond ++ " " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMAnd cond d n m)
    = "\tAND" ++ show cond ++ " " ++ show d ++ ", " ++ show n ++ ", " ++ show m
  show (ARMCmp cond d m)
    = "\tCMP" ++ show cond ++ " " ++ show d ++ ", " ++ show m
  show (ARMTst cond d m)
    = "\tTST" ++ show cond ++ " " ++ show d ++ ", " ++ show m
  show (ARMMov cond d n)
    = "\tMOV" ++ show cond ++ " " ++ show d ++ ", " ++ show n
  show (ARMMvn cond d n)
    = "\tMVN" ++ show cond ++ " " ++ show d ++ ", " ++ show n
  show (ARMNeg cond d n)
    = "\tNEG" ++ show cond ++ " " ++ show d ++ ", " ++ show n


  show (ARMSmull cond lo hi m s)
    = "\tSMULL" ++ show cond ++ " " ++
      show lo ++ ", " ++ show hi ++ ", " ++
      show m ++ ", " ++ show s

  show (ARMPUSH rs)
    = "\tPUSH {" ++ concat (intersperse ", " (map show rs)) ++ "}"
  show (ARMPOP rs)
    = "\tPOP {" ++ concat (intersperse ", " (map show rs)) ++ "}"
  show (ARMB cond xs)
    = "\tB" ++ show cond ++ " L" ++ show xs
  show (ARMBL xs)
    = "\tBL " ++ xs
