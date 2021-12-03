#include "tiger/regalloc/regalloc.h"

#include "tiger/output/logger.h"

extern frame::RegManager *reg_manager;

namespace ra {
/* TODO: Put your lab6 code here */

Result::~Result() {}

RegAllocator::RegAllocator(frame::Frame *frame,
                           std::unique_ptr<cg::AssemInstr> assem_instr) {}

void RegAllocator::RegAlloc() {}

std::unique_ptr<ra::Result> RegAllocator::TransferResult() { return nullptr; }
} // namespace ra