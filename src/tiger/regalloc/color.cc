#include "tiger/regalloc/color.h"

extern frame::RegManager *reg_manager;

namespace col {
Color::Color() {
  coloring = temp::Map::Empty();
  Initialize();
}

void Color::Initialize() {
  okColors.clear();
  temp::Map *temp_map = reg_manager->temp_map_;
  temp::TempList *registers = reg_manager->Registers();
  for (temp::Temp *temp : registers->GetList()) {
    std::string *color = temp_map->Look(temp);
    okColors.insert(color);
    coloring->Enter(temp, color); // precolored
  }
}

bool Color::Assign(live::INodePtr n) {
  if (okColors.empty()) {
    return false;
  }
  std::string *color = *(okColors.cbegin());
  coloring->Enter(n->NodeInfo(), color);
  return true;
}

void Color::MarkConflict(live::INodePtr n) {
  okColors.erase(coloring->Look(n->NodeInfo()));
}

void Color::MarkSameColor(live::INodePtr m, live::INodePtr n) {
  std::string *color = coloring->Look(n->NodeInfo());
  assert(color);
  coloring->Enter(m->NodeInfo(), color);
}

Result Color::GetResult() { return Result(coloring, nullptr); }

Color::~Color() {}
} // namespace col
