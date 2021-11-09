#include "tiger/semant/semant.h"
#include "tiger/absyn/absyn.h"
#include <iostream>
#include <map>

namespace absyn {

void AbsynTree::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                           err::ErrorMsg *errormsg) const {
  root_->SemAnalyze(venv, tenv, 0, errormsg);
}

type::Ty *SimpleVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  // If variable exists
  env::VarEntry *var = (env::VarEntry *)venv->Look(sym_);
  if (!var) {
    errormsg->Error(pos_, "undefined variable %s", sym_->Name().c_str());
    return type::NilTy::Instance();
  }
  return var->ty_;
}

type::Ty *FieldVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  // If variable is a record
  type::Ty *type = var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*type) != typeid(type::RecordTy)) {
    errormsg->Error(pos_, "not a record type");
    return type::NilTy::Instance();
  }

  type::RecordTy *record = static_cast<type::RecordTy *>(type);
  // If corresponding field exists
  for (type::Field *field : record->fields_->GetList()) {
    if (field->name_->Name() == sym_->Name()) {
      return field->ty_;
    }
  }
  errormsg->Error(pos_, "field %s doesn't exist", sym_->Name().c_str());
  return type::NilTy::Instance();
}

type::Ty *SubscriptVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   int labelcount,
                                   err::ErrorMsg *errormsg) const {
  // If variable is an array
  type::Ty *type = var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*type) != typeid(type::ArrayTy)) {
    errormsg->Error(pos_, "array type required");
    return type::NilTy::Instance();
  }

  type::ArrayTy *array = static_cast<type::ArrayTy *>(type);
  return array->ty_;
}

type::Ty *VarExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return var_->SemAnalyze(venv, tenv, labelcount, errormsg);
}

type::Ty *NilExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return type::NilTy::Instance();
}

type::Ty *IntExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  return type::IntTy::Instance();
}

type::Ty *StringExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  return type::StringTy::Instance();
}

type::Ty *CallExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  // If function is defined
  env::EnvEntry *entry = venv->Look(func_);
  if (!entry) {
    errormsg->Error(pos_, "undefined function %s", func_->Name().c_str());
    return type::VoidTy::Instance();
  }
  // If the name is a function
  if (typeid(*entry) != typeid(env::FunEntry)) {
    errormsg->Error(pos_, "function required");
    return type::NilTy::Instance();
  }
  env::FunEntry *function = static_cast<env::FunEntry *>(entry);
  const std::list<type::Ty *> &formalTys = function->formals_->GetList();
  auto formalTy = formalTys.cbegin();
  std::list<Exp *> args = args_->GetList();
  // If numbers of actual and formal parameters match
  if (args.size() > formalTys.size()) {
    errormsg->Error(pos_ - 1, "too many params in function " + func_->Name());
    return type::VoidTy::Instance();
  }
  for (Exp *arg : args) {
    type::Ty *type = arg->SemAnalyze(venv, tenv, labelcount, errormsg);
    if (errormsg->AnyErrors()) {
      break;
    }
    // If types of actual and formal parameters match
    if (!(*formalTy)->IsSameType(type)) {
      errormsg->Error(arg->pos_, "para type mismatch");
      return type::VoidTy::Instance();
    }
    ++formalTy;
  }
  return function->result_;
}

type::Ty *OpExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *leftTy = left_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *rightTy = right_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (errormsg->AnyErrors()) {
    return leftTy;
  }
  // If operands of arithmetic operators are integers
  if (oper_ < EQ_OP) {
    if (!leftTy->IsSameType(type::IntTy::Instance())) {
      errormsg->Error(left_->pos_, "integer required");
      return type::IntTy::Instance();
    }
    if (!rightTy->IsSameType(type::IntTy::Instance())) {
      errormsg->Error(right_->pos_, "integer required");
      return type::IntTy::Instance();
    }
  } else {
    // If types of operands match
    if (!leftTy->IsSameType(rightTy)) {
      errormsg->Error(pos_, "same type required");
      return leftTy;
    }
  }
  return leftTy;
}

type::Ty *RecordExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  // If corresponding record type is defined
  type::Ty *type = tenv->Look(typ_);
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", typ_->Name().c_str());
    return type::NilTy::Instance();
  }
  // If corresponding type is record
  type = type->ActualTy();
  if (typeid(*type) != typeid(type::RecordTy)) {
    errormsg->Error(pos_, "record type required");
    return type::NilTy::Instance();
  }
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", typ_->Name().c_str());
    return type::NilTy::Instance();
  }
  type::RecordTy *recordTy = static_cast<type::RecordTy *>(type);

  type::FieldList *fieldList =
      fields_->MakeFieldList(venv, tenv, labelcount, errormsg);
  // Handle "{}"
  if (fieldList->GetList().empty()) {
    return type::NilTy::Instance();
  }
  auto it = recordTy->fields_->GetList().cbegin();
  for (type::Field *field : fieldList->GetList()) {
    // If the order of fields in expression is same as that in type definition
    if (field->name_->Name() != (*it)->name_->Name()) {
      errormsg->Error(pos_, "field %s doesn't exist",
                      field->name_->Name().c_str());
    }
    // If the type of fields in expression is same as that in type definition
    if (!field->ty_->IsSameType((*it)->ty_)) {
      errormsg->Error(pos_, "type of field %s doesn't match",
                      field->name_->Name().c_str());
    }
    ++it;
  }
  return type;
}

type::Ty *SeqExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  type::TyList *types = seq_->SemAnalyze(venv, tenv, labelcount, errormsg);
  // The type of SeqExp is the type of the last expression
  return types->GetList().back();
}

type::Ty *AssignExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *varTy = var_->SemAnalyze(venv, tenv, labelcount, errormsg),
           *expTy = exp_->SemAnalyze(venv, tenv, labelcount, errormsg);
  // If value type matches variable type
  if (!varTy->IsSameType(expTy)) {
    errormsg->Error(pos_, typeid(*var_) == typeid(absyn::FieldVar)
                              ? "unmatched assign exp"
                              : "type mismatch");
  }
  // If loop variable is assigned
  if (typeid(*var_) == typeid(absyn::SimpleVar)) {
    absyn::SimpleVar *var = static_cast<absyn::SimpleVar *>(var_);
    env::VarEntry *entry = static_cast<env::VarEntry *>(venv->Look(var->sym_));
    if (entry->readonly_) {
      errormsg->Error(pos_, "loop variable can't be assigned");
    }
  }
  return varTy;
}

type::Ty *IfExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                            int labelcount, err::ErrorMsg *errormsg) const {
  test_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *thenTy = then_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (errormsg->AnyErrors()) {
    return thenTy;
  }
  if (elsee_) {
    type::Ty *elseTy = elsee_->SemAnalyze(venv, tenv, labelcount, errormsg);
    // If types of then expression and else expression match
    if (!thenTy->IsSameType(elseTy)) {
      errormsg->Error(pos_ - 1, "then exp and else exp type mismatch");
      return thenTy;
    }
  }
  // If the body of if-then expression produces no value
  if (!errormsg->AnyErrors() && !thenTy->IsSameType(type::VoidTy::Instance())) {
    errormsg->Error(then_->pos_, "if-then exp's body must produce no value");
  }
  return thenTy;
}

type::Ty *WhileExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  // If the body of while expression produces no value
  test_->SemAnalyze(venv, tenv, labelcount, errormsg);
  type::Ty *bodyTy = body_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!bodyTy->IsSameType(type::VoidTy::Instance())) {
    errormsg->Error(pos_, "while body must produce no value");
  }
  return type::VoidTy::Instance();
}

type::Ty *ForExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  venv->Enter(var_, new env::VarEntry(type::IntTy::Instance(), true));
  type::Ty *loTy = lo_->SemAnalyze(venv, tenv, labelcount, errormsg),
           *hiTy = hi_->SemAnalyze(venv, tenv, labelcount, errormsg);
  // If range type of for expression is integer
  if (!loTy->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(lo_->pos_, "for exp's range type is not integer");
  }
  if (!hiTy->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(hi_->pos_, "for exp's range type is not integer");
  }
  type::Ty *bodyTy = body_->SemAnalyze(venv, tenv, labelcount + 1, errormsg);
  venv->EndScope();
  return type::VoidTy::Instance();
}

type::Ty *BreakExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  // If break is inside a loop
  if (labelcount == 0) {
    errormsg->Error(pos_, "break is not inside any loop");
  }
  return type::VoidTy::Instance();
}

type::Ty *LetExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  for (absyn::Dec *dec : decs_->GetList()) {
    dec->SemAnalyze(venv, tenv, labelcount, errormsg);
  }
  type::Ty *type = body_->SemAnalyze(venv, tenv, labelcount, errormsg);
  venv->EndScope();
  return type;
}

type::Ty *ArrayExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  // If corresponding array type is defined
  type::Ty *type = tenv->Look(typ_);
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", typ_);
    return type::VoidTy::Instance();
  }
  // If corresponding type is array
  type = type->ActualTy();
  if (typeid(*type) != typeid(type::ArrayTy)) {
    errormsg->Error(pos_, "array type required");
    return type::VoidTy::Instance();
  }
  // If the type of elements in expression is same as that in type definition
  type::Ty *arrayTy = static_cast<type::ArrayTy *>(type)->ty_;
  type::Ty *initTy = init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (!initTy->IsSameType(arrayTy)) {
    errormsg->Error(pos_, "type mismatch");
    return type;
  }
  return new type::ArrayTy(type);
}

type::Ty *VoidExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                              int labelcount, err::ErrorMsg *errormsg) const {
  return type::VoidTy::Instance();
}

void FunctionDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                             int labelcount, err::ErrorMsg *errormsg) const {
  for (absyn::FunDec *function : functions_->GetList()) {
    type::TyList *formalTys =
        function->params_->MakeFormalTyList(tenv, errormsg);
    type::Ty *resultTy = function->result_ ? tenv->Look(function->result_)
                                           : type::VoidTy::Instance();
    // If two functions have the same name
    if (venv->Look(function->name_)) {
      errormsg->Error(function->pos_, "two functions have the same name");
      return;
    }
    venv->Enter(function->name_, new env::FunEntry(formalTys, resultTy));
  }
  for (absyn::FunDec *function : functions_->GetList()) {
    type::TyList *formalTys =
        function->params_->MakeFormalTyList(tenv, errormsg);
    type::Ty *resultTy = function->result_ ? tenv->Look(function->result_)
                                           : type::VoidTy::Instance();
    venv->BeginScope();
    // Append formal args to venv
    auto it = formalTys->GetList().cbegin();
    for (absyn::Field *param : function->params_->GetList()) {
      venv->Enter(param->name_, new env::VarEntry(*it));
      ++it;
    }
    type::Ty *returnTy =
        function->body_->SemAnalyze(venv, tenv, labelcount, errormsg);
    // If void function returns a value
    if (resultTy->IsSameType(type::VoidTy::Instance()) &&
        !returnTy->IsSameType(type::VoidTy::Instance())) {
      errormsg->Error(pos_, "procedure returns value");
    }
    venv->EndScope();
  }
}

void VarDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                        err::ErrorMsg *errormsg) const {
  assert(init_);
  type::Ty *type = nullptr,
           *initTy = init_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typ_) { // Explicit type
    type = tenv->Look(typ_);
    // If type is defined
    if (!type) {
      errormsg->Error(pos_, "undefined type %s", typ_->Name().c_str());
      return;
    }
    // If value type matches specified type
    if (!type->IsSameType(initTy)) {
      errormsg->Error(init_->pos_, "type mismatch");
    }
  } else { // Implicit type with initializer
    type = initTy;
    // If Nil value is used to initialize an implicit-type variable
    if (type->IsSameType(type::NilTy::Instance()) &&
        typeid(*type) != typeid(type::RecordTy)) {
      errormsg->Error(pos_, "init should not be nil without type specified");
      return;
    }
  }
  venv->Enter(var_, new env::VarEntry(type));
}

void TypeDec::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv, int labelcount,
                         err::ErrorMsg *errormsg) const {
  for (NameAndTy *type : types_->GetList()) {
    // If two types have the same name
    if (tenv->Look(type->name_)) {
      errormsg->Error(pos_, "two types have the same name");
      return;
    }
    tenv->Enter(type->name_, new type::NameTy(type->name_, nullptr));
  }
  // If type declarations form an illegal cycle
  for (NameAndTy *type : types_->GetList()) {
    type::NameTy *nameTy = static_cast<type::NameTy *>(tenv->Look(type->name_));
    assert(nameTy);
    nameTy->ty_ = type->ty_->SemAnalyze(tenv, errormsg);
    if (!nameTy->ty_) {
      errormsg->Error(pos_, "illegal type cycle");
      break;
    }
  }
  for (NameAndTy *type : types_->GetList()) {
    type::Ty *current = tenv->Look(type->name_);
    std::map<type::Ty *, int> map;
    int order = 0;
    while (current) {
      if (typeid(*current) != typeid(type::NameTy)) {
        break;
      }
      type::Ty *next = ((type::NameTy *)current)->ty_;
      if (next && next == current) {
        break;
      }
      auto it = map.find(current);
      if (it != map.end() && it->second < order) {
        errormsg->Error(pos_, "illegal type cycle");
        return;
      }
      map.insert(std::make_pair(current, order));
      ++order;
      current = next;
    }
  }
}

type::Ty *NameTy::SemAnalyze(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  return tenv->Look(name_);
}

type::Ty *RecordTy::SemAnalyze(env::TEnvPtr tenv,
                               err::ErrorMsg *errormsg) const {
  return new type::RecordTy(record_->MakeFieldList(tenv, errormsg));
}

type::Ty *ArrayTy::SemAnalyze(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  type::Ty *type = tenv->Look(array_);
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", array_->Name().c_str());
    return type::NilTy::Instance();
  }
  return new type::ArrayTy(type);
}

type::TyList *ExpList::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  int labelcount,
                                  err::ErrorMsg *errormsg) const {
  type::TyList *tyList = new type::TyList();
  for (absyn::Exp *exp : exp_list_) {
    type::Ty *ty = exp->SemAnalyze(venv, tenv, labelcount, errormsg);
    tyList->Append(ty);
  }
  return tyList;
}

type::FieldList *EFieldList::MakeFieldList(env::VEnvPtr venv, env::TEnvPtr tenv,
                                           int labelcount,
                                           err::ErrorMsg *errormsg) const {
  type::FieldList *fieldList = new type::FieldList();
  for (absyn::EField *efield : efield_list_) {
    type::Ty *ty = efield->exp_->SemAnalyze(venv, tenv, labelcount, errormsg);
    fieldList->Append(new type::Field(efield->name_, ty));
  }
  return fieldList;
}

} // namespace absyn

namespace sem {

void ProgSem::SemAnalyze() {
  FillBaseVEnv();
  FillBaseTEnv();
  absyn_tree_->SemAnalyze(venv_.get(), tenv_.get(), errormsg_.get());
}

} // namespace tr
