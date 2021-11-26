#include "straightline/slp.h"

#include <iostream>

namespace A {
int A::CompoundStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  return max(stm1->MaxArgs(), stm2->MaxArgs());
}

Table *A::CompoundStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  return stm2->Interp(stm1->Interp(t));
}

int A::AssignStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  return exp->MaxArgs();
}

Table *A::AssignStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  return t->Update(id, exp->Interp(t)->i);
}

int A::PrintStm::MaxArgs() const {
  // TODO: put your code here (lab1).
  return exps->MaxArgs();
}

Table *A::PrintStm::Interp(Table *t) const {
  // TODO: put your code here (lab1).
  exps->Print(t);
  return exps->Interp(t)->t;
}

int A::IdExp::MaxArgs() const { return 0; }

IntAndTable *A::IdExp::Interp(Table *t) const {
  return new IntAndTable(t->Lookup(id), t);
}

int A::NumExp::MaxArgs() const { return 0; }

IntAndTable *A::NumExp::Interp(Table *t) const {
  return new IntAndTable(num, t);
}

int A::OpExp::MaxArgs() const { return max(left->MaxArgs(), right->MaxArgs()); }

IntAndTable *A::OpExp::Interp(Table *t) const {
  IntAndTable *left_value = left->Interp(t);
  IntAndTable *right_value = right->Interp(left_value->t);
  switch (oper) {
  case PLUS:
    return new IntAndTable(left_value->i + right_value->i, right_value->t);
  case MINUS:
    return new IntAndTable(left_value->i - right_value->i, right_value->t);
  case TIMES:
    return new IntAndTable(left_value->i * right_value->i, right_value->t);
  case DIV:
    assert(right_value->i != 0);
    return new IntAndTable(left_value->i / right_value->i, right_value->t);
  }
}

int A::EseqExp::MaxArgs() const { return max(stm->MaxArgs(), exp->MaxArgs()); }

IntAndTable *A::EseqExp::Interp(Table *t) const {
  return exp->Interp(stm->Interp(t));
}

int A::PairExpList::MaxArgs() const {
  return max(NumExps(), max(exp->MaxArgs(), tail->MaxArgs()));
}

int A::PairExpList::NumExps() const { return 1 + tail->NumExps(); }

IntAndTable *A::PairExpList::Interp(Table *t) const {
  return tail->Interp(exp->Interp(t)->t);
}

void A::PairExpList::Print(Table *t) const {
  IntAndTable *result = exp->Interp(t);
  printf("%d ", result->i);
  tail->Print(result->t);
}

int A::LastExpList::MaxArgs() const { return exp->MaxArgs(); }

int A::LastExpList::NumExps() const { return 1; }

void A::LastExpList::Print(Table *t) const {
  printf("%d\n", exp->Interp(t)->i);
}

IntAndTable *A::LastExpList::Interp(Table *t) const { return exp->Interp(t); }

int Table::Lookup(const std::string &key) const {
  if (id == key) {
    return value;
  } else if (tail != nullptr) {
    return tail->Lookup(key);
  } else {
    assert(false);
  }
}

Table *Table::Update(const std::string &key, int val) const {
  return new Table(key, val, this);
}
} // namespace A
