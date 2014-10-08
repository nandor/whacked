module Whacked.Backend.ARM.ASM where


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
  deriving (Eq, Ord, Show)


data ASM
  = ARMLabel String
  | ARMAdd ARMReg ARMReg ARMReg
  | ARMSub ARMReg ARMReg ARMReg
  | ARMMul ARMReg ARMReg ARMReg
  | ARMSTM [ARMReg]
  | ARMLDM [ARMReg]
  | ARMMov ARMReg ARMReg
  | ARMLDR ARMReg Int
  deriving (Eq, Ord, Show)