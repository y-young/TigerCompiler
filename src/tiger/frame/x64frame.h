//
// Created by wzl on 2021/10/12.
//

#ifndef TIGER_COMPILER_X64FRAME_H
#define TIGER_COMPILER_X64FRAME_H

#include "tiger/frame/frame.h"

namespace frame {
class X64RegManager : public RegManager {
  /* TODO: Put your lab5 code here */
public:
  temp::TempList *Registers() { return nullptr; }
  temp::TempList *ArgRegs() { return nullptr; }
  temp::TempList *CallerSaves() { return nullptr; }
  temp::TempList *CalleeSaves() { return nullptr; }
  temp::TempList *ReturnSink() { return nullptr; }
  int WordSize() { return 8; }
  temp::Temp *FramePointer() { return nullptr; }
  temp::Temp *StackPointer() { return nullptr; }
  temp::Temp *ReturnValue() { return nullptr; }
};

} // namespace frame
#endif // TIGER_COMPILER_X64FRAME_H
