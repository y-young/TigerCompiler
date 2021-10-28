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
  env::VarEntry *var = (env::VarEntry *)venv->Look(sym_);
  if (!var) {
    errormsg->Error(pos_, "undefined variable %s", sym_->Name().c_str());
    return type::NilTy::Instance();
  }
  return var->ty_;
}

type::Ty *FieldVar::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  // test25.tig:5.4:not a record type
  type::Ty *type = var_->SemAnalyze(venv, tenv, labelcount, errormsg);
  if (typeid(*type) != typeid(type::RecordTy)) {
    errormsg->Error(pos_, "not a record type");
    return type::NilTy::Instance();
  }

  type::RecordTy *record = static_cast<type::RecordTy *>(type);
  // test22.tig:7.7:field nam doesn't exist
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
  // test24.tig:6.1:array type required
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
  // test19.tig:8.17:undefined variable a
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
  // test18.tig:5.19:undefined function do_nothing2
  env::FunEntry *function = (env::FunEntry *)venv->Look(func_);
  if (!function) {
    errormsg->Error(pos_, "undefined function %s", func_->Name().c_str());
    return type::VoidTy::Instance();
  }
  const std::list<type::Ty *> &formalTys = function->formals_->GetList();
  auto formalTy = formalTys.cbegin();
  std::list<Exp *> args = args_->GetList();
  // test36.tig:5.10:too many params in function g
  if (args.size() > formalTys.size()) {
    // args.resize(formalTys.size());
    errormsg->Error(pos_ - 1, "too many params in function " + func_->Name());
    return type::VoidTy::Instance();
  }
  for (Exp *arg : args) {
    type::Ty *type = arg->SemAnalyze(venv, tenv, labelcount, errormsg);
    if (errormsg->AnyErrors()) {
      break;
    }
    // test34.tig:5.8:para type mismatch
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
  // test21.tig:8.24:integer required
  // Check integer type for arithmetic operators
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
    // test13.tig:3.9:same type required
    if (!leftTy->IsSameType(rightTy)) {
      errormsg->Error(pos_, "same type required");
      return leftTy;
    }
  }
  return leftTy;
}

type::Ty *RecordExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                                int labelcount, err::ErrorMsg *errormsg) const {
  type::Ty *type = tenv->Look(typ_);
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", typ_->Name().c_str());
    return type::NilTy::Instance();
  }
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
  if (fieldList->GetList().empty()) {
    return type::NilTy::Instance();
  }
  auto it = recordTy->fields_->GetList().cbegin();
  for (type::Field *field : fieldList->GetList()) {
    if (field->name_->Name() != (*it)->name_->Name()) {
      errormsg->Error(pos_, "field %s doesn't exist",
                      field->name_->Name().c_str());
    }
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
  if (!varTy->IsSameType(expTy)) {
    // test23.tig:7.16:unmatched assign exp
    errormsg->Error(pos_, typeid(*var_) == typeid(absyn::FieldVar)
                              ? "unmatched assign exp"
                              : "type mismatch");
  }
  // test11.tig:3.12:loop variable can't be assigned
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
  // test15.tig:3.12:if-then exp's body must produce no value
  if (elsee_) {
    type::Ty *elseTy = elsee_->SemAnalyze(venv, tenv, labelcount, errormsg);
    // test9.tig:3.26:then exp and else exp type mismatch
    if (!thenTy->IsSameType(elseTy)) {
      errormsg->Error(pos_ - 1, "then exp and else exp type mismatch");
      return thenTy;
    }
  }
  if (!errormsg->AnyErrors() && !thenTy->IsSameType(type::VoidTy::Instance())) {
    errormsg->Error(then_->pos_, "if-then exp's body must produce no value");
  }
  return thenTy;
}

type::Ty *WhileExp::SemAnalyze(env::VEnvPtr venv, env::TEnvPtr tenv,
                               int labelcount, err::ErrorMsg *errormsg) const {
  // test10.tig:2.21:while body must produce no value
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
  // test11.tig:2.16:for exp's range type is not integer
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
  // test50.tig:8.4: break is not inside any loop
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
  type::Ty *type = tenv->Look(typ_);
  // tenv->Dump([](sym::Symbol *sym, type::Ty *ty) {
  //   printf("%s: ", sym->Name().c_str());
  //   std::cout << typeid(*ty).name() << ' ' << ty << ' '
  //             << ((type::NameTy *)ty)->ty_ << std::endl;
  // });
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", typ_);
    return type::VoidTy::Instance();
  }
  type = type->ActualTy();
  if (typeid(*type) != typeid(type::ArrayTy)) {
    errormsg->Error(pos_, "array type required");
    return type::VoidTy::Instance();
  }
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
    // test39.tig:6.2:two functions have the same name
    if (venv->Look(function->name_)) {
      errormsg->Error(function->pos_, "two functions have the same name");
      return;
    }
    venv->Enter(function->name_, new env::FunEntry(formalTys, resultTy));
  }
  for (absyn::FunDec *function : functions_->GetList()) {
    // TODO: no formal args
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
    // test21.tig:10.1:procedure returns value
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
    // test17.tig:4.31: undefined type treelist
    if (!type) {
      errormsg->Error(pos_, "undefined type %s", typ_->Name().c_str());
      return;
    }
    // test28.tig:7.51:type mismatch
    if (!type->IsSameType(initTy)) {
      errormsg->Error(init_->pos_, "type mismatch");
    }
  } else { // Implicit type with initializer
    type = initTy;
    // test45.tig:6.1:init should not be nil without type specified
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
    // test38.tig:5.11:two types have the same name
    if (tenv->Look(type->name_)) {
      errormsg->Error(pos_, "two types have the same name");
      return;
    }
    tenv->Enter(type->name_, new type::NameTy(type->name_, nullptr));
  }
  // test16.tig:4.8:illegal type cycle
  for (NameAndTy *type : types_->GetList()) {
    type::NameTy *nameTy = static_cast<type::NameTy *>(tenv->Look(type->name_));
    assert(nameTy);
    nameTy->ty_ = type->ty_->SemAnalyze(tenv, errormsg);
    if (!nameTy->ty_) {
      errormsg->Error(pos_, "illegal type cycle");
      break;
    }
  }
  // tenv->Dump([](sym::Symbol *sym, type::Ty *ty) {
  //   printf("%s: ", sym->Name().c_str());
  //   std::cout << typeid(*ty).name() << ' ' << ty << ' '
  //             << ((type::NameTy *)ty)->ty_ << std::endl;
  // });
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
      // std::cout << "next: " << next << " order: " << order << std::endl;
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
} // namespace sem
