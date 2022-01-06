#include "tiger/codegen/codegen.h"

#include <cassert>
#include <sstream>

extern frame::RegManager *reg_manager;

namespace {

constexpr int maxlen = 1024;

} // namespace

namespace cg {

void CodeGen::Codegen() {
  assem::InstrList *instr_list = new assem::InstrList();
  fs_ = frame_->GetLabel() + "_framesize";
  // Initialize frame pointer
  // instr_list->Append(new assem::OperInstr(
  //     "leaq " + fs_ + "(`s0), `d0",
  //     new temp::TempList(reg_manager->FramePointer()),
  //     new temp::TempList(reg_manager->StackPointer()), nullptr));

  for (tree::Stm *trace : traces_->GetStmList()->GetList()) {
    trace->Munch(*instr_list, fs_);
  }
  assem_instr_ =
      std::make_unique<AssemInstr>(frame::ProcEntryExit2(instr_list));
}

void AssemInstr::Print(FILE *out, temp::Map *map) const {
  for (auto instr : instr_list_->GetList())
    instr->Print(out, map);
  fprintf(out, "\n");
}

OperandType GetOperandType(const tree::Exp *exp) {
  if (typeid(*exp) == typeid(tree::ConstExp)) {
    return OperandType::IMM;
  }
  if (typeid(*exp) == typeid(tree::MemExp)) {
    return OperandType::MEM;
  }
  return OperandType::REG;
}

temp::TempList *MunchMemAddr(tree::Exp *addr, OperandRole role,
                             std::string &assem, assem::InstrList &instr_list,
                             std::string_view fs) {
  assem = role == SRC ? "(`s0)" : "(`d0)";
  if (typeid(*addr) == typeid(tree::BinopExp)) {
    // Does the address look like reg + const?
    tree::BinopExp *binopExp = static_cast<tree::BinopExp *>(addr);
    tree::Exp *left = binopExp->left_, *right = binopExp->right_;
    // IMM(%REG)
    if (typeid(*left) == typeid(tree::ConstExp)) {
      int offset = static_cast<const tree::ConstExp *>(left)->consti_;
      assem = std::to_string(offset) + assem;
      return new temp::TempList(right->Munch(instr_list, fs));
    }
    if (typeid(*right) == typeid(tree::ConstExp)) {
      int offset = static_cast<const tree::ConstExp *>(right)->consti_;
      assem = std::to_string(offset) + assem;
      return new temp::TempList(left->Munch(instr_list, fs));
    }
  }
  return new temp::TempList(addr->Munch(instr_list, fs));
}

temp::TempList *MunchOperand(tree::Exp *exp, OperandRole role,
                             std::string &assem, assem::InstrList &instr_list,
                             std::string_view fs) {
  OperandType type = GetOperandType(exp);
  switch (type) {
  case OperandType::IMM:
    assem =
        "$" + std::to_string(static_cast<const tree::ConstExp *>(exp)->consti_);
    return new temp::TempList();
  case OperandType::REG:
    assem = role == SRC ? "`s0" : "`d0";
    return new temp::TempList(exp->Munch(instr_list, fs));
  case OperandType::MEM:
    return MunchMemAddr(static_cast<const tree::MemExp *>(exp)->exp_, role,
                        assem, instr_list, fs);
  }
}
} // namespace cg

namespace tree {

void SeqStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  left_->Munch(instr_list, fs);
  right_->Munch(instr_list, fs);
}

void LabelStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  instr_list.Append(new assem::LabelInstr(label_->Name(), label_));
}

void JumpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  std::stringstream assem;
  assem << "jmp " << exp_->name_->Name();
  instr_list.Append(new assem::OperInstr(assem.str(), nullptr, nullptr,
                                         new assem::Targets(jumps_)));
}

void CjumpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  const std::string instructions[] = {"je",  "jne", "jl",   "jg",  "jle",
                                      "jge", "jnb", "jnbe", "jna", "jnae"};
  const std::string instruction = instructions[op_];
  temp::Temp *left = left_->Munch(instr_list, fs),
             *right = right_->Munch(instr_list, fs);
  instr_list.Append(new assem::OperInstr(
      "cmpq `s1, `s0", nullptr, new temp::TempList({left, right}), nullptr));
  std::vector<temp::Label *> *labels =
      new std::vector<temp::Label *>{true_label_, false_label_};
  instr_list.Append(new assem::OperInstr(instruction + " `j0", nullptr, nullptr,
                                         new assem::Targets(labels)));
}

void MoveStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  tree::Exp *src = src_, *dst = dst_;
  std::string srcAssem, dstAssem;
  temp::TempList *srcTemps = nullptr,
                 *dstTemps = cg::MunchOperand(dst, cg::OperandRole::DST,
                                              dstAssem, instr_list, fs);
  if (typeid(*src) == typeid(tree::MemExp) &&
      typeid(*dst) == typeid(tree::MemExp)) {
    // Moving from memory to memory is not supported,
    // use an intermediate register to store value loaded for source.
    temp::TempList *intermediate =
        new temp::TempList(src->Munch(instr_list, fs));
    // Transform the instruction, modify src to the intermediate register
    src = new tree::TempExp(intermediate->GetList().front());
  }

  // Now the instruction contains at most one memory operand
  srcTemps =
      cg::MunchOperand(src, cg::OperandRole::SRC, srcAssem, instr_list, fs);
  if (typeid(*dst) == typeid(tree::MemExp)) {
    // In instructions like movq $0, (%rsi), %rsi is not used as destination
    // but is used as source
    dstAssem = dstAssem.substr(0, dstAssem.find_first_of('(')) + "(`s" +
               std::to_string(srcTemps->GetList().size()) + ")";
    srcTemps->Append(dstTemps->GetList().front());
    dstTemps = nullptr;
  }
  std::stringstream assem;
  assem << "movq " << srcAssem << ", " << dstAssem;
  if (typeid(*src) == typeid(tree::MemExp) ||
      typeid(*dst) == typeid(tree::MemExp)) {
    // If any of the operands is memory, the move won't be between two
    // temps, so it doesn't need to be considered during register allocation
    instr_list.Append(
        new assem::OperInstr(assem.str(), dstTemps, srcTemps, nullptr));
  } else {
    instr_list.Append(new assem::MoveInstr(assem.str(), dstTemps, srcTemps));
  }
}

void ExpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  exp_->Munch(instr_list, fs);
}

temp::Temp *BinopExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  temp::Temp *result = nullptr;
  const std::string instructions[] = {"addq", "subq", "imulq", "idivq", "andq",
                                      "orq",  "salq", "shrq",  "sarq",  "xorq"};
  const std::string instruction = instructions[op_];
  std::stringstream assem;
  std::string leftAssem, rightAssem;
  temp::TempList *left = nullptr, *right = nullptr;

  if (op_ == BinOp::MUL_OP || op_ == BinOp::DIV_OP) {
    // imul and idiv don't support immediate operand, so use a temp register
    left = new temp::TempList(left_->Munch(instr_list, fs));
    right = new temp::TempList(right_->Munch(instr_list, fs));
    leftAssem = rightAssem = "`s0";
    // imul and idiv use %rax as the destination
    result = reg_manager->ReturnValue();
  } else {
    left = cg::MunchOperand(left_, cg::OperandRole::SRC, leftAssem, instr_list,
                            fs);
    right = cg::MunchOperand(right_, cg::OperandRole::SRC, rightAssem,
                             instr_list, fs);
    result = temp::TempFactory::NewTemp();
  }

  // Move an operand into register
  // Destination is also used as a operand
  right->Append(result);
  temp::TempList *dst = new temp::TempList(result);
  assem << "movq " << leftAssem << ", `d0";
  if (typeid(*left_) == typeid(tree::MemExp)) {
    instr_list.Append(new assem::OperInstr(assem.str(), dst, left, nullptr));
  } else {
    instr_list.Append(new assem::MoveInstr(assem.str(), dst, left));
  }
  assem.str("");
  // Avoid Float point exception
  if (op_ == BinOp::DIV_OP) {
    instr_list.Append(new assem::OperInstr(
        "cltd",
        new temp::TempList({reg_manager->GetRegister(cg::IMUL_RAX),
                            reg_manager->GetRegister(cg::IMUL_RDX)}),
        new temp::TempList(reg_manager->GetRegister(cg::IMUL_RAX)), nullptr));
  }

  // Now build the instruction to do the calculation
  if (op_ == BinOp::MUL_OP || op_ == BinOp::DIV_OP) {
    // imul and idiv use %rax as the destination
    assem << instruction << ' ' << rightAssem;
    // %rdx is used as destination in imulq and idivq
    dst->Append(reg_manager->GetRegister(cg::IMUL_RDX));
  } else {
    assem << instruction << ' ' << rightAssem << ", `d0";
  }
  instr_list.Append(new assem::OperInstr(assem.str(), dst, right, nullptr));
  return result;
}

temp::Temp *MemExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  temp::Temp *result = temp::TempFactory::NewTemp(),
             *addrReg = temp::TempFactory::NewTemp();
  std::string addrAssem;
  temp::TempList *addrSrc =
      cg::MunchMemAddr(exp_, cg::OperandRole::SRC, addrAssem, instr_list, fs);
  std::stringstream assem;
  assem << "movq " << addrAssem << ", `d0";
  instr_list.Append(new assem::OperInstr(
      assem.str(), new temp::TempList(result), addrSrc, nullptr));
  return result;
}

temp::Temp *TempExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  if (temp_ == reg_manager->FramePointer()) {
    temp::Temp *fp = temp::TempFactory::NewTemp();
    std::stringstream assem;
    assem << "leaq " << fs << "(`s0), `d0";
    instr_list.Append(new assem::OperInstr(
        assem.str(), new temp::TempList(fp),
        new temp::TempList(reg_manager->StackPointer()), nullptr));
    return fp;
  }
  return temp_;
}

temp::Temp *EseqExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  stm_->Munch(instr_list, fs);
  return exp_->Munch(instr_list, fs);
}

temp::Temp *NameExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  temp::Temp *dst = temp::TempFactory::NewTemp();
  std::stringstream assem;
  assem << "leaq " << name_->Name() << "(%rip), `d0";
  instr_list.Append(new assem::OperInstr(assem.str(), new temp::TempList(dst),
                                         nullptr, nullptr));
  return dst;
}

temp::Temp *ConstExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  temp::Temp *dst = temp::TempFactory::NewTemp();
  std::stringstream assem;
  assem << "movq $" << consti_ << ", `d0";
  instr_list.Append(new assem::OperInstr(assem.str(), new temp::TempList(dst),
                                         nullptr, nullptr));
  return dst;
}

temp::Temp *CallExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  assert(typeid(*fun_) == typeid(NameExp));
  NameExp *function = static_cast<NameExp *>(fun_);
  temp::TempList *argRegs = args_->MunchArgs(instr_list, fs);
  // CALL instruction will trash caller saved registers,
  // specified as destinations
  instr_list.Append(new assem::OperInstr("callq " + function->name_->Name(),
                                         reg_manager->CallerSaves(),
                                         reg_manager->ArgRegs(), nullptr));
  temp::Temp *returnValue = temp::TempFactory::NewTemp();
  instr_list.Append(
      new assem::MoveInstr("movq `s0, `d0", new temp::TempList(returnValue),
                           new temp::TempList(reg_manager->ReturnValue())));
  const int argCount = args_->GetList().size(),
            extraStack = frame::ArgExtraStack(argCount);
  if (extraStack > 0) {
    // Some arguments were passed on the stack, restore the stack pointer
    std::stringstream assem;
    assem << "addq $" << extraStack << ", `d0";
    instr_list.Append(new assem::OperInstr(
        assem.str(), new temp::TempList(reg_manager->StackPointer()), nullptr,
        nullptr));
  }
  return returnValue;
}

temp::TempList *ExpList::MunchArgs(assem::InstrList &instr_list,
                                   std::string_view fs) {
  temp::TempList *argRegs = reg_manager->ArgRegs();
  const int argRegNum = argRegs->GetList().size();
  temp::TempList *argRegsUsed = new temp::TempList();
  int pos = 0;
  for (Exp *exp : exp_list_) {
    temp::Temp *srcReg = exp->Munch(instr_list, fs);
    temp::TempList *src = new temp::TempList(srcReg);
    if (pos < argRegNum) {
      // Pass argument in registers
      temp::Temp *argReg = argRegs->NthTemp(pos);
      argRegsUsed->Append(argReg);
      instr_list.Append(new assem::MoveInstr("movq `s0, `d0",
                                             new temp::TempList(argReg), src));
    } else {
      // Pass argument on stack
      // instr_list.Append(new assem::OperInstr(
      //     "pushq `s0", nullptr, src, nullptr));
      // Tiger Interpreter hasn't support pushq instruction yet,
      // so we'll implement it ourselves
      instr_list.Append(new assem::OperInstr(
          "subq $8, `d0", new temp::TempList(reg_manager->StackPointer()),
          nullptr, nullptr));
      instr_list.Append(new assem::OperInstr(
          "movq `s0, (%rsp)", new temp::TempList(reg_manager->StackPointer()),
          src, nullptr));
    }
    ++pos;
  }
  return argRegsUsed;
}

} // namespace tree
