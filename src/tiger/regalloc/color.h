#ifndef TIGER_COMPILER_COLOR_H
#define TIGER_COMPILER_COLOR_H

#include "tiger/codegen/assem.h"
#include "tiger/frame/temp.h"
#include "tiger/liveness/liveness.h"
#include "tiger/util/graph.h"

#include <set>

namespace col {
struct Result {
  Result() : coloring(nullptr), spills(nullptr) {}
  Result(temp::Map *coloring, live::INodeListPtr spills)
      : coloring(coloring), spills(spills) {}
  temp::Map *coloring;
  live::INodeListPtr spills;
};

class Color {
private:
  std::set<std::string *> okColors;
  temp::Map *coloring;

public:
  Color();
  // initialize colors
  void Initialize();
  // assign a color, return false if spilled
  bool Assign(live::INodePtr n);
  // mark two nodes as the same color
  void MarkSameColor(live::INodePtr m, live::INodePtr n);
  // mark a conflict with another node
  void MarkConflict(live::INodePtr n);
  Result GetResult();
  ~Color();
};
} // namespace col

#endif // TIGER_COMPILER_COLOR_H
