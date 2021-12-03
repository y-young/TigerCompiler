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
  // Save callee-saved registers
  temp::TempList *calleeSaved = new temp::TempList();
  for (auto reg : reg_manager->CalleeSaves()->GetList()) {
    temp::Temp *dst = temp::TempFactory::NewTemp();
    instr_list->Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList(dst), new temp::TempList(reg)));
    calleeSaved->Append(dst);
  }

  fs_ = frame_->GetLabel() + "_framesize";
  // Initialize frame pointer
  instr_list->Append(
      new assem::MoveInstr("leaq " + fs_ + "(`s0), `d0",
                           new temp::TempList(reg_manager->FramePointer()),
                           new temp::TempList(reg_manager->StackPointer())));

  for (tree::Stm *trace : traces_->GetStmList()->GetList()) {
    trace->Munch(*instr_list, fs_);
  }

  // Restore callee-saved registers
  auto saved = calleeSaved->GetList().cbegin();
  for (auto reg : reg_manager->CalleeSaves()->GetList()) {
    instr_list->Append(new assem::MoveInstr(
        "movq `s0, `d0", new temp::TempList(reg), new temp::TempList(*saved)));
    ++saved;
  }
  delete calleeSaved;

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
    return nullptr;
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
  std::string srcAssem, dstAssemm;
  temp::TempList *src = nullptr,
                 *dst = cg::MunchOperand(dst_, cg::OperandRole::DST, dstAssemm,
                                         instr_list, fs);
  if (typeid(*src_) == typeid(tree::MemExp) &&
      typeid(*dst_) == typeid(tree::MemExp)) {
    // Moving from memory to memory is not supported,
    // use an intermediate register to store value loaded for source.
    src = new temp::TempList(src_->Munch(instr_list, fs));
    srcAssem = "`s0";
  } else {
    src =
        cg::MunchOperand(src_, cg::OperandRole::SRC, srcAssem, instr_list, fs);
  }
  std::stringstream assem;
  assem << "movq " << srcAssem << ", " << dstAssemm;
  instr_list.Append(new assem::OperInstr(assem.str(), dst, src, nullptr));
}

void ExpStm::Munch(assem::InstrList &instr_list, std::string_view fs) {
  exp_->Munch(instr_list, fs);
}

temp::Temp *BinopExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  temp::Temp *result = nullptr;
  const std::string instructions[] = {"addq", "subq", "imul", "idiv", "andq",
                                      "orq",  "salq", "shrq", "sarq", "xorq"};
  const std::string instruction = instructions[op_];
  std::stringstream assem;
  std::string leftAssem, rightAssem;
  temp::TempList *left = cg::MunchOperand(left_, cg::OperandRole::SRC,
                                          leftAssem, instr_list, fs),
                 *right = cg::MunchOperand(right_, cg::OperandRole::SRC,
                                           rightAssem, instr_list, fs);
  if (op_ == BinOp::MUL_OP || op_ == BinOp::DIV_OP) {
    // imul and idiv use %rax as the destination
    result = reg_manager->ReturnValue();
  } else {
    result = temp::TempFactory::NewTemp();
  }
  temp::TempList *dst = new temp::TempList(result);
  assem << "movq " << leftAssem << ", `d0";
  instr_list.Append(new assem::MoveInstr(assem.str(), dst, left));
  assem.str("");
  if (op_ == BinOp::MUL_OP || op_ == BinOp::DIV_OP) {
    // imul and idiv use %rax as the destination
    assem << instruction << ' ' << rightAssem;
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
  instr_list.Append(
      new assem::MoveInstr(assem.str(), new temp::TempList(result), addrSrc));
  return result;
}

temp::Temp *TempExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
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
  instr_list.Append(
      new assem::MoveInstr(assem.str(), new temp::TempList(dst), nullptr));
  return dst;
}

temp::Temp *ConstExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  temp::Temp *dst = temp::TempFactory::NewTemp();
  std::stringstream assem;
  assem << "movq $" << consti_ << ", `d0";
  instr_list.Append(
      new assem::MoveInstr(assem.str(), new temp::TempList(dst), nullptr));
  return dst;
}

temp::Temp *CallExp::Munch(assem::InstrList &instr_list, std::string_view fs) {
  assert(typeid(*fun_) == typeid(NameExp));
  NameExp *function = static_cast<NameExp *>(fun_);
  temp::TempList *args = args_->MunchArgs(instr_list, fs);
  // CALL instruction will trash caller saved registers,
  // specified as destinations
  instr_list.Append(new assem::OperInstr("callq " + function->name_->Name(),
                                         reg_manager->CallerSaves(), args,
                                         nullptr));
  const int argCount = args_->GetList().size(),
            extraStack = frame::ArgExtraStack(argCount);
  if (extraStack > 0) {
    // Some arguments were passed on the stack, restore the stack pointer
    std::stringstream assem;
    assem << "addq $" << extraStack << ", `d0";
    instr_list.Append(new assem::MoveInstr(
        assem.str(), new temp::TempList(reg_manager->StackPointer()), nullptr));
  }
  return reg_manager->ReturnValue();
}

temp::TempList *ExpList::MunchArgs(assem::InstrList &instr_list,
                                   std::string_view fs) {
  temp::TempList *argRegs = reg_manager->ArgRegs();
  const int argRegNum = argRegs->GetList().size();
  temp::TempList *argsUsed = new temp::TempList();
  int pos = 0;
  for (Exp *exp : exp_list_) {
    temp::Temp *srcReg = exp->Munch(instr_list, fs);
    argsUsed->Append(srcReg);
    temp::TempList *src = new temp::TempList(srcReg);
    if (pos < argRegNum) {
      // Pass argument in registers
      temp::Temp *argReg = argRegs->NthTemp(pos);
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
      instr_list.Append(
          new assem::OperInstr("movq `s0, (%rsp)", nullptr, src, nullptr));
    }
    ++pos;
  }
  return argsUsed;
}

} // namespace tree
