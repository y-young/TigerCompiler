//
// Created by wzl on 2021/10/12.
//

#ifndef TIGER_COMPILER_X64FRAME_H
#define TIGER_COMPILER_X64FRAME_H

#include "tiger/frame/frame.h"

namespace frame {
class X64RegManager : public RegManager {
private:
  const std::string REG_NAMES[16] = {"rax", "rbx", "rcx", "rdx", "rsi", "rdi",
                                     "rbp", "rsp", "r8",  "r9",  "r10", "r11",
                                     "r12", "r13", "r14", "r15"};
  const std::vector<int> ARG_REGS = {5, 4, 3, 2, 8, 9};
  const std::vector<int> CALLER_SAVED_REGS = {10, 11};
  const std::vector<int> CALLEE_SAVED_REGS = {1, 6, 12, 13, 14, 15};
  const int FRAME_POINTER_REG = 6;
  const int STACK_POINTER_REG = 7;
  const int RETURN_VALUE_REG = 0;
  const int WORD_SIZE = 8;

public:
  X64RegManager();
  temp::TempList *Registers();
  temp::TempList *ArgRegs();
  temp::TempList *CallerSaves();
  temp::TempList *CalleeSaves();
  temp::TempList *ReturnSink();
  int WordSize() { return WORD_SIZE; }
  temp::Temp *FramePointer();
  temp::Temp *StackPointer();
  temp::Temp *ReturnValue();
};

class InFrameAccess : public Access {
public:
  int offset;

  explicit InFrameAccess(int offset) : offset(offset) {}
  tree::Exp *ToExp(tree::Exp *framePointer);
};

class InRegAccess : public Access {
public:
  temp::Temp *reg;

  explicit InRegAccess(temp::Temp *reg) : reg(reg) {}
  tree::Exp *ToExp(tree::Exp *framePointer);
};

class X64Frame : public Frame {
public:
  X64Frame(temp::Label *name, std::unique_ptr<BoolList> escapes);
  int AllocLocal();
  AccessList *Formals();
  int Size() const;
};

// Calculate extra stack size needed by arguments
int ArgExtraStack(int argCount);

} // namespace frame
#endif // TIGER_COMPILER_X64FRAME_H
