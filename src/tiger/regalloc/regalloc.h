#ifndef TIGER_REGALLOC_REGALLOC_H_
#define TIGER_REGALLOC_REGALLOC_H_

#include "tiger/codegen/assem.h"
#include "tiger/codegen/codegen.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/regalloc/color.h"
#include "tiger/util/graph.h"

#include <sstream>
#include <stack>

namespace ra {

class Result {
public:
  temp::Map *coloring_;
  assem::InstrList *il_;

  Result() : coloring_(nullptr), il_(nullptr) {}
  Result(temp::Map *coloring, assem::InstrList *il)
      : coloring_(coloring), il_(il) {}
  Result(const Result &result) = delete;
  Result(Result &&result) = delete;
  Result &operator=(const Result &result) = delete;
  Result &operator=(Result &&result) = delete;
  ~Result();
};

class RegAllocator {
private:
  frame::Frame *frame_;
  std::unique_ptr<ra::Result> result_;
  assem::InstrList *instr_list;

  live::IGraphPtr interf_graph;
  live::MoveList *moves;
  tab::Table<temp::Temp, live::INode> *temp_node_map;
  int K;

  // ----- Node work lists, sets and stacks -----
  // machine registers, preassigned a color
  live::INodeListPtr precolored;
  // temporary registers, not precolored and not yet processed
  live::INodeListPtr initial;
  // list of low-degree non-move-related nodes
  live::INodeListPtr simplify_worklist;
  // low-degree move-related nodes
  live::INodeListPtr freeze_worklist;
  // high-degree nodes
  live::INodeListPtr spill_worklist;
  // nodes marked for spilling during this round; initially empty
  live::INodeListPtr spilled_nodes;
  /**
   * registers that have been coalesced; when u<-v is coalesced,
   * v is added to this set and u put back on some work-list (or vice versa)
   */
  live::INodeListPtr coalesced_nodes;
  // nodes successfully colored
  live::INodeListPtr colored_nodes;
  // stack containing temporaries removed from the graph
  live::INodeListPtr select_stack;
  // Short live range Registers that should not be spilled
  live::INodeListPtr nospill_temps;

  // ----- Move sets -----
  // moves that have been coalesced
  live::MoveList *coalesced_moves;
  // moves whose source and target interfere
  live::MoveList *constrained_moves;
  // moves that will no longer be considered for coalescing
  live::MoveList *frozen_moves;
  // moves enabled for possible coalescing
  live::MoveList *worklist_moves;
  // moves not yet ready for coalescing
  live::MoveList *active_moves;

  // ----- Other data structures -----
  // an array containing the current degree of each node
  tab::Table<live::INode, int> *degree;
  // a mapping from a node to the list of moves it is associated with
  tab::Table<live::INode, live::MoveList> *movelist;
  // when a move (u, v) has been coalesced, and v put in coalesced_nodes,
  // then alias[v] = u
  tab::Table<live::INode, live::INode> *alias;
  // the color chosen by the algorithm for a node;
  // for precolored nodes this is initialized to the given color
  // tab::Table<live::INode, std::string> *color;

  void Build();
  void AddEdge(live::INodePtr u, live::INodePtr v);
  void MakeWorklist();
  live::INodeListPtr Adjacent(live::INodePtr n);
  live::MoveList *NodeMoves(live::INodePtr n);
  bool MoveRelated(live::INodePtr n);
  void Simplify();
  void DecrementDegree(live::INodePtr m);
  void EnableMoves(live::INodeListPtr nodes);
  void Coalesce();
  void AddWorkList(live::INodePtr u);
  bool OK(live::INodePtr t, live::INodePtr r);
  bool Conservative(live::INodeListPtr nodes);
  live::INodePtr GetAlias(live::INodePtr n);
  void Combine(live::INodePtr u, live::INodePtr v);
  void Freeze();
  void FreezeMoves(live::INodePtr u);
  void SelectSpill();
  col::Result AssignColors();
  void RewriteProgram();
  assem::Instr *ReplaceTemp(assem::Instr *instr, temp::Temp *oldTemp,
                            temp::Temp *newTemp);

  inline int Degree(live::INodePtr n) { return *(degree->Look(n)); }
  // Strip moves where src and dst are the same
  assem::InstrList *Strip(temp::Map *color) const;
  // If a move instruction is unneeded
  bool UnneededMove(assem::MoveInstr *instr, temp::Map *color) const;
  void Construct();
  void Destruct();

public:
  RegAllocator(frame::Frame *frame,
               std::unique_ptr<cg::AssemInstr> assem_instr);
  void RegAlloc();
  std::unique_ptr<ra::Result> TransferResult() { return std::move(result_); }
  ~RegAllocator();
};

} // namespace ra

#endif