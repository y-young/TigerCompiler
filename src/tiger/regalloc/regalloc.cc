#include "tiger/regalloc/regalloc.h"

#include "tiger/output/logger.h"

extern frame::RegManager *reg_manager;

namespace ra {

Result::~Result() {}

void RegAllocator::Build() {
  Destruct();
  Construct();

  temp::Map *temp_map = reg_manager->temp_map_;
  for (live::INodePtr node : interf_graph->Nodes()->GetList()) {
    degree->Enter(node, new int(node->OutDegree()));

    live::MoveList *relatedMoves = new live::MoveList();
    for (auto move : worklist_moves->GetList()) {
      live::INodePtr src = move.first, dst = move.second;
      if (src == node || dst == node) {
        relatedMoves->Append(src, dst);
      }
    }
    movelist->Enter(node, relatedMoves);

    alias->Enter(node, node);
    std::string *precolor = temp_map->Look(node->NodeInfo());
    if (!precolor) {
      initial->Append(node);
    } else {
      precolored->Append(node);
    }
  }
}

void RegAllocator::AddEdge(live::INodePtr u, live::INodePtr v) {
  if (!u->Adj(v) && u != v) {
    if (!precolored->Contain(u)) {
      interf_graph->AddEdge(u, v);
      ++(*degree->Look(u));
    }
    if (!precolored->Contain(v)) {
      interf_graph->AddEdge(v, u);
      ++(*degree->Look(v));
    }
  }
}

void RegAllocator::MakeWorklist() {
  for (live::INodePtr n : initial->GetList()) {
    if (Degree(n) >= K) {
      spill_worklist->Union(n);
    } else if (MoveRelated(n)) {
      freeze_worklist->Union(n);
    } else {
      simplify_worklist->Union(n);
    }
  }
  initial->Clear();
}

live::INodeListPtr RegAllocator::Adjacent(live::INodePtr n) {
  live::INodeListPtr adj_list = n->Succ();
  return adj_list->Diff(select_stack->Union(coalesced_nodes));
}

live::MoveList *RegAllocator::NodeMoves(live::INodePtr n) {
  return movelist->Look(n)->Intersect(active_moves->Union(worklist_moves));
}

bool RegAllocator::MoveRelated(live::INodePtr n) {
  return !NodeMoves(n)->Empty();
}

void RegAllocator::Simplify() {
  if (simplify_worklist->Empty()) {
    return;
  }
  live::INodePtr n = simplify_worklist->GetList().front();
  simplify_worklist->DeleteNode(n);
  select_stack->Prepend(n);
  for (live::INodePtr m : Adjacent(n)->GetList()) {
    DecrementDegree(m);
  }
}

void RegAllocator::DecrementDegree(live::INodePtr m) {
  if (precolored->Contain(m)) {
    return;
  }
  int *d = degree->Look(m);
  // degree[m] <- d - 1
  --(*d);
  if (*d == K) {
    // EnableMoves({m} ∪ Adjacent(m))
    live::INodeListPtr m_set = new live::INodeList(m);
    EnableMoves(m_set->Union(Adjacent(m)));
    delete m_set;
    spill_worklist->DeleteNode(m);
    if (MoveRelated(m)) {
      freeze_worklist->Union(m);
    } else {
      simplify_worklist->Union(m);
    }
  }
}

void RegAllocator::EnableMoves(live::INodeListPtr nodes) {
  for (live::INodePtr n : nodes->GetList()) {
    for (auto m : NodeMoves(n)->GetList()) {
      live::INodePtr src = m.first, dst = m.second;
      if (active_moves->Contain(src, dst)) {
        active_moves->Delete(src, dst);
        worklist_moves->Union(src, dst);
      }
    }
  }
}

void RegAllocator::Coalesce() {
  if (worklist_moves->Empty()) {
    return;
  }
  auto m = worklist_moves->GetList().front();
  live::INodePtr x = m.first, y = m.second;
  x = GetAlias(x);
  y = GetAlias(y);
  live::INodePtr u, v;
  if (precolored->Contain(y)) {
    u = y, v = x;
  } else {
    u = x, v = y;
  }
  worklist_moves->Delete(m.first, m.second);
  live::INodeListPtr adj_u = Adjacent(u), adj_v = Adjacent(v);
  if (u == v) {
    coalesced_moves->Union(x, y);
    AddWorkList(u);
  } else if (precolored->Contain(v) || u->Adj(v)) {
    constrained_moves->Union(x, y);
    AddWorkList(u);
    AddWorkList(v);
  } else if ((precolored->Contain(u) &&
              std::all_of(adj_v->GetList().begin(), adj_v->GetList().end(),
                          [this, &u](live::INodePtr t) { return OK(t, u); })) ||
             (!precolored->Contain(u) && Conservative(adj_u->Union(adj_v)))) {
    coalesced_moves->Union(x, y);
    Combine(u, v);
    AddWorkList(u);
  } else {
    active_moves->Union(x, y);
  }
}

void RegAllocator::AddWorkList(live::INodePtr u) {
  if (!precolored->Contain(u) && !MoveRelated(u) && Degree(u) < K) {
    freeze_worklist->DeleteNode(u);
    simplify_worklist->Union(u);
  }
}

bool RegAllocator::OK(live::INodePtr t, live::INodePtr r) {
  return Degree(t) < K || precolored->Contain(t) || t->Adj(r);
}

bool RegAllocator::Conservative(live::INodeListPtr nodes) {
  int k = 0;
  for (live::INodePtr n : nodes->GetList()) {
    if (Degree(n) >= K) {
      ++k;
    }
  }
  return k < K;
}

live::INodePtr RegAllocator::GetAlias(live::INodePtr n) {
  if (coalesced_nodes->Contain(n)) {
    return GetAlias(alias->Look(n));
  }
  return n;
}

void RegAllocator::Combine(live::INodePtr u, live::INodePtr v) {
  if (freeze_worklist->Contain(v)) {
    freeze_worklist->DeleteNode(v);
  } else {
    spill_worklist->DeleteNode(v);
  }
  coalesced_nodes->Union(v);
  alias->Set(v, u);
  movelist->Set(u, movelist->Look(u)->Union(movelist->Look(v)));
  EnableMoves(new live::INodeList(v));
  for (live::INodePtr t : Adjacent(v)->GetList()) {
    AddEdge(t, u);
    DecrementDegree(t);
  }
  if (Degree(u) >= K && freeze_worklist->Contain(u)) {
    freeze_worklist->DeleteNode(u);
    spill_worklist->Union(u);
  }
}

void RegAllocator::Freeze() {
  if (freeze_worklist->Empty()) {
    return;
  }
  live::INodePtr u = freeze_worklist->GetList().front();
  freeze_worklist->DeleteNode(u);
  simplify_worklist->Union(u);
  FreezeMoves(u);
}

void RegAllocator::FreezeMoves(live::INodePtr u) {
  for (auto m : NodeMoves(u)->GetList()) {
    live::INodePtr x = m.first, y = m.second, v;
    if (GetAlias(y) == GetAlias(u)) {
      v = GetAlias(x);
    } else {
      v = GetAlias(y);
    }
    active_moves->Delete(x, y);
    frozen_moves->Union(x, y);
    if (NodeMoves(v)->Empty() && Degree(v) < K) {
      freeze_worklist->DeleteNode(v);
      simplify_worklist->Union(v);
    }
  }
}

void RegAllocator::SelectSpill() {
  if (spill_worklist->Empty()) {
    return;
  }
  // TODO: using heuristic
  live::INodePtr m = spill_worklist->GetList().front();
  spill_worklist->DeleteNode(m);
  if (nospill_temps->Contain(m)) {
    return;
  }
  simplify_worklist->Union(m);
  FreezeMoves(m);
}

col::Result RegAllocator::AssignColors() {
  col::Color coloring;
  while (!select_stack->Empty()) {
    // n = pop(select_stack)
    live::INodePtr n = select_stack->GetList().front();
    select_stack->DeleteNode(n);
    coloring.Initialize();
    for (live::INodePtr w : n->Succ()->GetList()) {
      live::INodePtr alias = GetAlias(w);
      if (colored_nodes->Union(precolored)->Contain(alias)) {
        coloring.MarkConflict(alias);
      }
    }
    if (coloring.Assign(n)) {
      colored_nodes->Union(n);
    } else {
      spilled_nodes->Union(n);
    }
  }
  for (live::INodePtr n : coalesced_nodes->GetList()) {
    coloring.MarkSameColor(n, GetAlias(n));
  }
  return coloring.GetResult();
}

void RegAllocator::RewriteProgram() {
  live::INodeListPtr new_temps = new live::INodeList();
  nospill_temps->Clear();
  for (live::INodePtr v : spilled_nodes->GetList()) {
    // allocate memory locations for each v in spilled_nodes
    frame::InFrameAccess *access = static_cast<frame::InFrameAccess *>(
        frame::Access::AllocLocal(frame_, true));
    // create a new temporary for each definition and each use
    temp::Temp *temp = temp::TempFactory::NewTemp();
    assem::InstrList *new_instr_list = new assem::InstrList();
    for (assem::Instr *instr : instr_list->GetList()) {
      assert(!precolored->Contain(v));
      instr->ReplaceTemp(v->NodeInfo(), temp);
      if (instr->Use()->Contain(temp)) {
        // insert a fetch before each use of v_i
        std::stringstream assem;
        assem << "movq (" << frame_->name_->Name() << "_framesize"
              << frame_->Offset() << ")(`s0), `d0";
        assem::OperInstr *fetch_instr = new assem::OperInstr(
            assem.str(), new temp::TempList(temp),
            new temp::TempList(reg_manager->StackPointer()), nullptr);
        new_instr_list->Append(fetch_instr);
      }
      new_instr_list->Append(instr);
      if (instr->Def()->Contain(temp)) {
        // insert a store after each definition of v_i
        std::stringstream assem;
        assem << "movq `s0, (" << frame_->name_->Name() << "_framesize"
              << frame_->Offset() << ")(`d0)";
        assem::OperInstr *store_instr = new assem::OperInstr(
            assem.str(), new temp::TempList(reg_manager->StackPointer()),
            new temp::TempList(temp), nullptr);
        new_instr_list->Append(store_instr);
      }
    }
    live::INodePtr new_node = interf_graph->NewNode(temp);
    new_temps->Append(new_node);
    nospill_temps->Append(new_node);
    instr_list = new_instr_list;
  }

  spill_worklist->Clear();
  delete initial;
  initial = colored_nodes->Union(coalesced_nodes->Union(new_temps));
  colored_nodes->Clear();
  coalesced_nodes->Clear();
}

void RegAllocator::RegAlloc() {
  // Live analysis
  fg::FlowGraphFactory flowGraphFactory(instr_list);
  flowGraphFactory.AssemFlowGraph();
  fg::FGraphPtr flowgraph = flowGraphFactory.GetFlowGraph();

  live::LiveGraphFactory liveGraphFactory(flowgraph);
  liveGraphFactory.Liveness();
  live::LiveGraph livegraph = liveGraphFactory.GetLiveGraph();
  interf_graph = livegraph.interf_graph;
  moves = livegraph.moves;
  worklist_moves = moves;
  temp_node_map = liveGraphFactory.GetTempNodeMap();

  Build();
  MakeWorklist();
  while (!simplify_worklist->Empty() || !worklist_moves->Empty() ||
         !freeze_worklist->Empty() || !spill_worklist->Empty()) {
    if (!simplify_worklist->Empty()) {
      Simplify();
    } else if (!worklist_moves->Empty()) {
      Coalesce();
    } else if (!freeze_worklist->Empty()) {
      Freeze();
    } else if (!spill_worklist->Empty()) {
      SelectSpill();
    }
  }
  col::Result coloring_result = AssignColors();
  if (!spilled_nodes->Empty()) {
    RewriteProgram();
    RegAlloc();
  } else {
    result_ = std::make_unique<Result>(coloring_result.coloring,
                                       Strip(coloring_result.coloring));
    temp::Map *color =
        temp::Map::LayerMap(reg_manager->temp_map_, coloring_result.coloring);
  }
}

assem::InstrList *RegAllocator::Strip(temp::Map *color) const {
  assem::InstrList *result = new assem::InstrList();
  for (assem::Instr *instr : instr_list->GetList()) {
    if (typeid(*instr) == typeid(assem::MoveInstr) &&
        UnneededMove(static_cast<assem::MoveInstr *>(instr), color)) {
      continue;
    }
    result->Append(instr);
  }
  return result;
}

bool RegAllocator::UnneededMove(assem::MoveInstr *instr,
                                temp::Map *color) const {
  temp::TempList *src = instr->src_, *dst = instr->dst_;
  if (!src || !dst) {
    return false;
  }
  std::set<std::string *> left, right;
  for (temp::Temp *temp : src->GetList()) {
    left.insert(color->Look(temp));
  }
  for (temp::Temp *temp : dst->GetList()) {
    right.insert(color->Look(temp));
  }
  return left == right;
}

void RegAllocator::Construct() {
  precolored = new live::INodeList();
  initial = new live::INodeList();
  simplify_worklist = new live::INodeList();
  freeze_worklist = new live::INodeList();
  spill_worklist = new live::INodeList();
  spilled_nodes = new live::INodeList();
  coalesced_nodes = new live::INodeList();
  colored_nodes = new live::INodeList();
  select_stack = new live::INodeList();
  nospill_temps = new live::INodeList();

  coalesced_moves = new live::MoveList();
  constrained_moves = new live::MoveList();
  frozen_moves = new live::MoveList();
  active_moves = new live::MoveList();

  degree = new tab::Table<live::INode, int>();
  movelist = new tab::Table<live::INode, live::MoveList>();
  alias = new tab::Table<live::INode, live::INode>();
}

void RegAllocator::Destruct() {
  delete precolored;
  delete initial;
  delete simplify_worklist;
  delete freeze_worklist;
  delete spill_worklist;
  delete spilled_nodes;
  delete coalesced_nodes;
  delete colored_nodes;
  delete select_stack;

  delete coalesced_moves;
  delete constrained_moves;
  delete frozen_moves;
  delete active_moves;

  delete degree;
  delete movelist;
  delete alias;
}

RegAllocator::RegAllocator(frame::Frame *frame,
                           std::unique_ptr<cg::AssemInstr> assem_instr)
    : frame_(frame), instr_list(assem_instr->GetInstrList()) {
  K = reg_manager->Registers()->GetList().size();
  Construct();
  nospill_temps = nullptr;
  worklist_moves = nullptr;
}

RegAllocator::~RegAllocator() {
  Destruct();
  delete nospill_temps;
  delete worklist_moves;
}
} // namespace ra