#include "tiger/liveness/flowgraph.h"

namespace fg {

void FlowGraphFactory::AssemFlowGraph() {
  assem::Instr *prevInstr = nullptr;
  fg::FNodePtr prevNode = nullptr;
  // First round: Match labels with instructions,
  // create node for every instruction and add edges for non-jump ones
  for (assem::Instr *instr : instr_list_->GetList()) {
    fg::FNodePtr node = flowgraph_->NewNode(instr);
    if (prevInstr) {
      if (typeid(*prevInstr) == typeid(assem::LabelInstr)) {
        assem::LabelInstr *labelInstr =
            static_cast<assem::LabelInstr *>(prevInstr);
        label_map_->Enter(labelInstr->label_, node);
        flowgraph_->AddEdge(prevNode, node);
      } else if (typeid(*prevInstr) == typeid(assem::OperInstr)) {
        // If previous instruction is not a jump, add an edge
        assem::OperInstr *operInstr =
            static_cast<assem::OperInstr *>(prevInstr);
        if (!operInstr->jumps_) {
          flowgraph_->AddEdge(prevNode, node);
        }
      } else { // MoveInstr
        flowgraph_->AddEdge(prevNode, node);
      }
    }
    prevInstr = instr;
    prevNode = node;
  }
  // Second round: Add edges for jump instructions
  for (FNodePtr node : flowgraph_->Nodes()->GetList()) {
    assem::Instr *instr = node->NodeInfo();
    if (typeid(*instr) != typeid(assem::OperInstr)) {
      continue;
    }
    assem::OperInstr *operInstr = static_cast<assem::OperInstr *>(instr);
    if (!operInstr->jumps_) {
      continue;
    }
    assem::Targets *targets = operInstr->jumps_;
    for (temp::Label *target : *(targets->labels_)) {
      fg::FNodePtr targetNode = label_map_->Look(target);
      flowgraph_->AddEdge(node, targetNode);
    }
  }
}

} // namespace fg

namespace assem {

temp::TempList *LabelInstr::Def() const { return new temp::TempList(); }

temp::TempList *MoveInstr::Def() const {
  if (!dst_) {
    return new temp::TempList();
  }
  return dst_;
}

temp::TempList *OperInstr::Def() const {
  if (!dst_) {
    return new temp::TempList();
  }
  return dst_;
}

temp::TempList *LabelInstr::Use() const { return new temp::TempList(); }

temp::TempList *MoveInstr::Use() const {
  if (!src_) {
    return new temp::TempList();
  }
  return src_;
}

temp::TempList *OperInstr::Use() const {
  if (!src_) {
    return new temp::TempList();
  }
  return src_;
}
} // namespace assem
