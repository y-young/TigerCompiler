#include "tiger/frame/x64frame.h"

extern frame::RegManager *reg_manager;

namespace frame {

X64RegManager::X64RegManager() : RegManager() {
  for (std::string name : REG_NAMES) {
    temp::Temp *reg = temp::TempFactory::NewTemp();
    temp_map_->Enter(reg, new std::string("%" + name));
    regs_.push_back(reg);
  }
}

temp::TempList *X64RegManager::Registers() {
  temp::TempList *list = new temp::TempList();
  for (int regno = 0; regno < 16; ++regno) {
    if (regno == STACK_POINTER_REG) { // rsp
      continue;
    }
    list->Append(regs_[regno]);
  }
  return list;
}

temp::TempList *X64RegManager::ArgRegs() {
  temp::TempList *list = new temp::TempList();
  for (int regno : ARG_REGS) {
    list->Append(regs_[regno]);
  }
  return list;
}

temp::TempList *X64RegManager::CallerSaves() {
  temp::TempList *list = new temp::TempList();
  for (int regno : CALLER_SAVED_REGS) {
    list->Append(regs_[regno]);
  }
  return list;
}

temp::TempList *X64RegManager::CalleeSaves() {
  temp::TempList *list = new temp::TempList();
  for (int regno : CALLEE_SAVED_REGS) {
    list->Append(regs_[regno]);
  }
  return list;
}

temp::TempList *X64RegManager::ReturnSink() {
  temp::TempList *tempList = CalleeSaves();
  tempList->Append(StackPointer());
  tempList->Append(ReturnValue());
  return tempList;
}

temp::Temp *X64RegManager::FramePointer() { return regs_[FRAME_POINTER_REG]; }

temp::Temp *X64RegManager::StackPointer() { return regs_[STACK_POINTER_REG]; }

temp::Temp *X64RegManager::ReturnValue() { return regs_[RETURN_VALUE_REG]; }

Access *Access::AllocLocal(Frame *frame, bool escape) {
  if (escape) {
    return new frame::InFrameAccess(frame->AllocLocal());
  } else {
    return new frame::InRegAccess(temp::TempFactory::NewTemp());
  }
}

tree::Exp *InFrameAccess::ToExp(tree::Exp *framePointer) {
  return new tree::MemExp(new tree::BinopExp(tree::BinOp::PLUS_OP, framePointer,
                                             new tree::ConstExp(offset)));
}

tree::Exp *InRegAccess::ToExp(tree::Exp *framePointer) {
  return new tree::TempExp(reg);
}

X64Frame::X64Frame(temp::Label *name, std::unique_ptr<BoolList> escapes)
    : Frame(name) {
  formals = new AccessList();
  for (bool escape : *escapes) {
    formals->push_back(Access::AllocLocal(this, escape));
  }
}

int X64Frame::Offset() const { return offset; }

int X64Frame::AllocLocal() {
  // Keep away from the return address on the top of the frame
  offset -= reg_manager->WordSize();
  return offset;
}

AccessList *X64Frame::Formals() { return formals; }

int X64Frame::Size() const {
  const int argCount = formals->size();
  return -offset + ArgExtraStack(argCount);
}

tree::Exp *ExternalCall(std::string name, tree::ExpList *args) {
  return new tree::CallExp(
      new tree::NameExp(temp::LabelFactory::NamedLabel(name)), args);
}

tree::Stm *ProcEntryExit1(Frame *frame, tree::Stm *stm) {
  tree::Stm *prologue = new tree::ExpStm(new tree::ConstExp(0));
  // Save callee-saved registers
  temp::TempList *calleeSaved = new temp::TempList();
  for (auto reg : reg_manager->CalleeSaves()->GetList()) {
    temp::Temp *dst = temp::TempFactory::NewTemp();
    prologue =
        new tree::SeqStm(prologue, new tree::MoveStm(new tree::TempExp(dst),
                                                     new tree::TempExp(reg)));
    calleeSaved->Append(dst);
  }

  // View shift
  tree::SeqStm *viewShift = nullptr, *tail = nullptr;
  temp::TempList *argRegs = reg_manager->ArgRegs();
  const int argRegCount = argRegs->GetList().size(),
            argCount = frame->Formals()->size();
  int argPos = 0;
  for (Access *formal : *(frame->Formals())) {
    tree::Exp *inFrameDst =
                  formal->ToExp(new tree::TempExp(reg_manager->FramePointer())),
              *src = nullptr;
    if (argPos >= argRegCount) {
      // Get from stack
      const int offset = (argCount - argPos) * reg_manager->WordSize();
      src = new tree::MemExp(new tree::BinopExp(
          tree::BinOp::PLUS_OP, new tree::TempExp(reg_manager->FramePointer()),
          new tree::ConstExp(offset)));
    } else {
      // Get from argument registers
      src = new tree::TempExp(argRegs->NthTemp(argPos));
    }
    prologue = new tree::SeqStm(prologue, new tree::MoveStm(inFrameDst, src));
    ++argPos;
  }

  stm = new tree::SeqStm(prologue, stm);

  // Restore callee-saved registers
  auto saved = calleeSaved->GetList().cbegin();
  for (auto reg : reg_manager->CalleeSaves()->GetList()) {
    stm = new tree::SeqStm(stm, new tree::MoveStm(new tree::TempExp(reg),
                                                  new tree::TempExp(*saved)));
    ++saved;
  }
  delete calleeSaved;

  return stm;
}

assem::InstrList *ProcEntryExit2(assem::InstrList *body) {
  body->Append(new assem::OperInstr("", new temp::TempList(),
                                    reg_manager->ReturnSink(), nullptr));
  return body;
}

assem::Proc *ProcEntryExit3(Frame *frame, assem::InstrList *body) {
  std::stringstream prologue;
  const std::string name = temp::LabelFactory::LabelString(frame->name_);
  const int rspOffset = frame->Size();
  prologue << ".set " << name << "_framesize, " << rspOffset << std::endl;
  prologue << name << ":" << std::endl;
  prologue << "subq $" << rspOffset << ", %rsp" << std::endl;
  std::stringstream epilogue;
  epilogue << "addq $" << rspOffset << ", %rsp" << std::endl;
  epilogue << "retq" << std::endl;
  return new assem::Proc(prologue.str(), body, epilogue.str());
}

int ArgExtraStack(int argCount) {
  const int argRegsCount = reg_manager->ArgRegs()->GetList().size();
  return std::max(argCount - argRegsCount, 0) * reg_manager->WordSize();
}

} // namespace frame