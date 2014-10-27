{-# LANGUAGE QuasiQuotes #-}
module Whacked.Backend.ARM.ASM where


import Data.List
import Whacked.Scratch
import Whacked.Types
import Text.Heredoc



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
  | ARMCore SCore
  | ARMLtorg
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
      ".ascii " ++ show string ++ "\n" ++
      ".byte 0"
  show ARMLtorg
    = ".ltorg"

  show (ARMLoadConst d const)
    = "    LDR " ++ show d ++ ", =" ++ show const
  show (ARMLdr d base off)
    = "    LDR " ++ show d ++ ", [" ++ show base ++ ", " ++ show off ++ "]"
  show (ARMStr d base off)
    = "    STR " ++ show d ++ ", [" ++ show base ++ ", " ++ show off ++ "]"
  show (ARMLdrb d base off)
    = "    LDRB " ++ show d ++ ", [" ++ show base ++ ", " ++ show off ++ "]"
  show (ARMStrb d base off)
    = "    STRB " ++ show d ++ ", [" ++ show base ++ ", " ++ show off ++ "]"
  show (ARMAdr d label)
    = "    LDR " ++ show d ++ ", =" ++ label
  show (ARMLdrLsl d base reg lsl)
    = "    LDR " ++ show d ++ ", [" ++ show base ++ ", " ++
      show reg ++ ", lsl #" ++ show lsl ++ "]"
  show (ARMStrLsl d base reg lsl)
    = "    STR " ++ show d ++ ", [" ++ show base ++ ", " ++
      show reg ++ ", lsl #" ++ show lsl ++ "]"


  show (ARMAdd cond d n m)
    = "    ADD" ++ show cond ++ " " ++ show d ++ ", " ++ show n ++
      ", " ++ show m
  show (ARMSub cond d n m)
    = "    SUB" ++ show cond ++ " " ++ show d ++ ", " ++ show n ++
      ", " ++ show m
  show (ARMOrr cond d n m)
    = "    ORR" ++ show cond ++ " " ++ show d ++ ", " ++ show n ++
      ", " ++ show m
  show (ARMAnd cond d n m)
    = "    AND" ++ show cond ++ " " ++ show d ++ ", " ++ show n ++
      ", " ++ show m
  show (ARMCmp cond d m)
    = "    CMP" ++ show cond ++ " " ++ show d ++ ", " ++ show m
  show (ARMTst cond d m)
    = "    TST" ++ show cond ++ " " ++ show d ++ ", " ++ show m
  show (ARMMov cond d n)
    = "    MOV" ++ show cond ++ " " ++ show d ++ ", " ++ show n
  show (ARMMvn cond d n)
    = "    MVN" ++ show cond ++ " " ++ show d ++ ", " ++ show n
  show (ARMNeg cond d n)
    = "    NEG" ++ show cond ++ " " ++ show d ++ ", " ++ show n


  show (ARMSmull cond lo hi m s)
    = "    SMULL" ++ show cond ++ " " ++
      show lo ++ ", " ++ show hi ++ ", " ++
      show m ++ ", " ++ show s

  show (ARMPUSH rs)
    = "    PUSH {" ++ concat (intersperse ", " (map show rs)) ++ "}"
  show (ARMPOP rs)
    = "    POP {" ++ concat (intersperse ", " (map show rs)) ++ "}"
  show (ARMB cond xs)
    = "    B" ++ show cond ++ " L" ++ show xs
  show (ARMBL xs)
    = "    BL " ++ xs

  show (ARMCore SReadInt)
    = [str|__read_int:
          |    PUSH {LR}
          |    LDR R0, =1f
          |    SUB R1, SP, #4
          |    BL scanf
          |    LDR r0, [SP,#-4]
          |    POP {PC}
          |  1:
          |    .asciz "%d"
          |    .align 4
          |]

  show (ARMCore SReadChar)
    = [str|__read_char:
          |    B getchar
          |]

  show (ARMCore SPrintInt)
    = [str|__print_int:
          |    MOV R1, R0
          |    LDR R0, =1f
          |    B printf
          |  1:
          |    .asciz "%d"
          |    .align 4
          |]

  show (ARMCore SPrintChar)
    = [str|__print_char:
          |    MOV R1, R0
          |    LDR R0, =1f
          |    B printf
          |  1:
          |    .asciz "%c"
          |    .align 4
          |]

  show (ARMCore SPrintBool)
    = [str|__print_bool:
          |    TST R0, #1
          |    LDRNE R0, =1f
          |    LDREQ R0, =2f
          |    B printf
          |  1:
          |    .asciz "true"
          |  2:
          |    .asciz "false"
          |    .align 4
          |]


  show (ARMCore SPrintString)
    = [str|__print_string:
          |   B printf
          |]

  show (ARMCore SPrintRef)
    = [str|__print_ref:
          |    MOV R1, R0
          |    LDR R0, =1f
          |    B printf
          |  1:
          |    .asciz "%p"
          |    .align 4
          |]

  show (ARMCore SAlloc)
    = [str|__alloc:
          |   PUSH {LR}
          |   ADD R0, R0, #4
          |   BL malloc
          |   ADD R0, R0, #4
          |   POP {PC}
          |]

  show (ARMCore SDelete)
    = [str|__delete:
          |   PUSH {LR}
          |   SUB R0, R0, #4
          |   BL free
          |   MOV R0, #0
          |   POP {PC}
          |]