#include "tiger/liveness/liveness.h"

extern frame::RegManager *reg_manager;

namespace live {

bool MoveList::Contain(INodePtr src, INodePtr dst) {
  return std::any_of(move_list_.cbegin(), move_list_.cend(),
                     [src, dst](std::pair<INodePtr, INodePtr> move) {
                       return move.first == src && move.second == dst;
                     });
}

void MoveList::Delete(INodePtr src, INodePtr dst) {
  assert(src && dst);
  auto move_it = move_list_.begin();
  for (; move_it != move_list_.end(); move_it++) {
    if (move_it->first == src && move_it->second == dst) {
      break;
    }
  }
  assert(move_it != move_list_.end());
  move_list_.erase(move_it);
}

MoveList *MoveList::Union(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : move_list_) {
    res->move_list_.push_back(move);
  }
  for (auto move : list->GetList()) {
    if (!Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

void MoveList::Union(INodePtr src, INodePtr dst) {
  if (!Contain(src, dst)) {
    move_list_.push_back(std::make_pair(src, dst));
  }
}

MoveList *MoveList::Intersect(MoveList *list) {
  auto *res = new MoveList();
  for (auto move : list->GetList()) {
    if (Contain(move.first, move.second))
      res->move_list_.push_back(move);
  }
  return res;
}

void MoveList::DeleteNode(INodePtr node) {
  move_list_.remove_if([node](std::pair<INodePtr, INodePtr> move) {
    return move.first == node || move.second == node;
  });
}

void LiveGraphFactory::LiveMap() {
  const std::list<fg::FNodePtr> &nodes = flowgraph_->Nodes()->GetList();
  for (fg::FNodePtr node : nodes) {
    assem::Instr *instr = node->NodeInfo();
    in_->Enter(node, new temp::TempList());
    out_->Enter(node, new temp::TempList());
  }

  bool fixed = false;
  while (!fixed) {
    fixed = true;
    for (auto it = nodes.crbegin(); it != nodes.crend(); ++it) {
      fg::FNodePtr node = *it;
      assem::Instr *instr = node->NodeInfo();
      temp::TempList *use = instr->Use(), *def = instr->Def();
      // out[n] = ∪(s∈succ[n]) in[s]
      temp::TempList *out = new temp::TempList();
      for (fg::FNodePtr succ : node->Succ()->GetList()) {
        out = out->Union(in_->Look(succ));
      }
      // in[n] = use[n] ∪ (out[n] - def[n])
      temp::TempList *in = use->Union(out_->Look(node)->Subtract(def));
      if (!in->Equal(in_->Look(node)) || !out->Equal(out_->Look(node))) {
        fixed = false;
        in_->Set(node, in);
        out_->Set(node, out);
      }
    }
  }
}

void LiveGraphFactory::InterfGraph() {
  PrecoloredInterf();
  const std::list<fg::FNodePtr> &nodes = flowgraph_->Nodes()->GetList();
  // Precreate nodes for all temporary registers
  for (fg::FNodePtr node : nodes) {
    assem::Instr *instr = node->NodeInfo();
    for (temp::Temp *temp : instr->Use()->GetList()) {
      if (!temp_node_map_->Look(temp)) {
        INodePtr node = live_graph_.interf_graph->NewNode(temp);
        temp_node_map_->Enter(temp, node);
      }
    }
    for (temp::Temp *temp : instr->Def()->GetList()) {
      if (!temp_node_map_->Look(temp)) {
        INodePtr node = live_graph_.interf_graph->NewNode(temp);
        temp_node_map_->Enter(temp, node);
      }
    }
  }
  // Build interference graph
  for (fg::FNodePtr node : nodes) {
    assem::Instr *instr = node->NodeInfo();
    temp::TempList *out = out_->Look(node);
    // For each new definition
    for (temp::Temp *def : instr->Def()->GetList()) {
      INodePtr def_node = temp_node_map_->Look(def);
      if (typeid(*instr) == typeid(assem::MoveInstr)) {
        // At a move instruction def <- c, where variables b1,...,bj are
        // live-out, add interference edges (def, b1), (def, b2), ..., (def, bj)
        // for any bi that is **not** the same as c.
        for (temp::Temp *b : out->Subtract(instr->Use())->GetList()) {
          INodePtr b_node = temp_node_map_->Look(b);
          live_graph_.interf_graph->AddEdge(def_node, b_node);
          live_graph_.interf_graph->AddEdge(b_node, def_node);
        }

        for (temp::Temp *use : instr->Use()->GetList()) {
          INodePtr use_node = temp_node_map_->Look(use);
          live_graph_.moves->Union(use_node, def_node);
        }
      } else {
        // At any nonmove instruction that defines a variable def,
        // where the live-out variables are b1,...,bj,
        // add interference edges (def, b1), (def, b2), ..., (def, bj)
        for (temp::Temp *b : out->GetList()) {
          INodePtr b_node = temp_node_map_->Look(b);
          live_graph_.interf_graph->AddEdge(def_node, b_node);
          live_graph_.interf_graph->AddEdge(b_node, def_node);
        }
      }
    }
  }
}

void LiveGraphFactory::PrecoloredInterf() {
  const std::list<temp::Temp *> &temps = reg_manager->Registers()->GetList();
  for (temp::Temp *temp : temps) {
    INodePtr node = live_graph_.interf_graph->NewNode(temp);
    temp_node_map_->Enter(temp, node);
  }
  for (temp::Temp *temp1 : temps) {
    for (temp::Temp *temp2 : temps) {
      if (temp1 != temp2) {
        INodePtr node1 = temp_node_map_->Look(temp1);
        INodePtr node2 = temp_node_map_->Look(temp2);
        live_graph_.interf_graph->AddEdge(node1, node2);
      }
    }
  }
}

void LiveGraphFactory::Liveness() {
  LiveMap();
  InterfGraph();
}

} // namespace live