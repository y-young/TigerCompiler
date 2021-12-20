#include "tiger/frame/temp.h"

#include <cstdio>
#include <set>
#include <sstream>

namespace temp {

LabelFactory LabelFactory::label_factory;
TempFactory TempFactory::temp_factory;

Label *LabelFactory::NewLabel() {
  char buf[100];
  sprintf(buf, "L%d", label_factory.label_id_++);
  return NamedLabel(std::string(buf));
}

/**
 * Get symbol of a label_. The label_ will be created only if it is not found.
 * @param s label_ string
 * @return symbol
 */
Label *LabelFactory::NamedLabel(std::string_view s) {
  return sym::Symbol::UniqueSymbol(s);
}

std::string LabelFactory::LabelString(Label *s) { return s->Name(); }

Temp *TempFactory::NewTemp() {
  Temp *p = new Temp(temp_factory.temp_id_++);
  std::stringstream stream;
  stream << 't';
  stream << p->num_;
  Map::Name()->Enter(p, new std::string(stream.str()));

  return p;
}

int Temp::Int() const { return num_; }

Map *Map::Empty() { return new Map(); }

Map *Map::Name() {
  static Map *m = nullptr;
  if (!m)
    m = Empty();
  return m;
}

Map *Map::LayerMap(Map *over, Map *under) {
  if (over == nullptr)
    return under;
  else
    return new Map(over->tab_, LayerMap(over->under_, under));
}

void Map::Enter(Temp *t, std::string *s) {
  assert(tab_);
  tab_->Enter(t, s);
}

std::string *Map::Look(Temp *t) {
  std::string *s;
  assert(tab_);
  s = tab_->Look(t);
  if (s)
    return s;
  else if (under_)
    return under_->Look(t);
  else
    return nullptr;
}

void Map::DumpMap(FILE *out) {
  tab_->Dump([out](temp::Temp *t, std::string *r) {
    fprintf(out, "t%d -> %s\n", t->Int(), r->data());
  });
  if (under_) {
    fprintf(out, "---------\n");
    under_->DumpMap(out);
  }
}

bool TempList::Contain(Temp *t) const {
  return std::any_of(temp_list_.cbegin(), temp_list_.cend(),
                     [t](Temp *t1) { return t1 == t; });
}

TempList *TempList::Union(TempList *other) const {
  TempList *res = new TempList();
  for (Temp *temp : temp_list_) {
    if (!res->Contain(temp)) {
      res->temp_list_.push_back(temp);
    }
  }
  for (Temp *temp : other->GetList()) {
    if (!res->Contain(temp)) {
      res->temp_list_.push_back(temp);
    }
  }
  return res;
}

TempList *TempList::Subtract(TempList *other) const {
  TempList *res = new TempList();
  for (Temp *temp : temp_list_) {
    if (!other->Contain(temp)) {
      res->temp_list_.push_back(temp);
    }
  }
  return res;
}

bool TempList::Equal(TempList *other) const {
  std::set<Temp *> left, right;
  for (Temp *temp : temp_list_) {
    left.insert(temp);
  }
  for (Temp *temp : other->GetList()) {
    right.insert(temp);
  }
  return left == right;
}

TempList *TempList::Replace(Temp *oldTemp, Temp *newTemp) const {
  TempList *res = new TempList();
  for (Temp *temp : temp_list_) {
    if (temp == oldTemp) {
      res->temp_list_.push_back(newTemp);
    } else {
      res->temp_list_.push_back(temp);
    }
  }
  return res;
}

void TempList::Print(FILE *out) const {
  for (Temp *temp : temp_list_) {
    fprintf(out, "t%d ", temp->Int());
  }
  fprintf(out, "\n");
}

} // namespace temp