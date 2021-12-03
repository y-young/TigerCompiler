#include "tiger/translate/translate.h"

#include <map>
#include <tiger/absyn/absyn.h>

#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/frame.h"
#include "tiger/frame/temp.h"
#include "tiger/frame/x64frame.h"

extern frame::Frags *frags;
extern frame::RegManager *reg_manager;

namespace tr {

typedef std::list<Access *> AccessList;

Access *Access::AllocLocal(Level *level, bool escape) {
  return new Access(level, frame::Access::AllocLocal(level->frame_, escape));
}

tree::Exp *Access::ToExp(Level *currentLevel) {
  tree::Exp *framePointer = currentLevel->StaticLink(level_);
  return access_->ToExp(framePointer);
}

Level::Level(Level *parent, temp::Label *name,
             std::unique_ptr<BoolList> escapes)
    : parent_(parent) {
  // Add an extra formal parameter for static link
  escapes->push_front(true);
  frame_ = new frame::X64Frame(name, std::move(escapes));
}

AccessList *Level::Formals() {
  frame::AccessList *list = frame_->Formals();
  auto access = list->cbegin();
  ++access; // Skip static link
  AccessList *formals = new AccessList();
  while (access != list->cend()) {
    formals->push_back(new Access(this, *access));
    ++access;
  }
  return formals;
}

std::unique_ptr<Level> Level::Outermost() {
  return std::make_unique<Level>(nullptr,
                                 temp::LabelFactory::NamedLabel("tigermain"),
                                 std::make_unique<BoolList>());
}

tree::Exp *Level::StaticLink(Level *targetLevel) {
  tree::Exp *framePointer = new tree::TempExp(reg_manager->FramePointer());
  Level *currentLevel = this;
  while (currentLevel && currentLevel != targetLevel) {
    framePointer =
        currentLevel->frame_->Formals()->front()->ToExp(framePointer);
    currentLevel = currentLevel->parent_;
  }
  // targetlevel must be the ancestor of currentLevel
  assert(currentLevel);
  return framePointer;
}

class Cx {
public:
  temp::Label **trues_;
  temp::Label **falses_;
  tree::Stm *stm_;

  Cx(temp::Label **trues, temp::Label **falses, tree::Stm *stm)
      : trues_(trues), falses_(falses), stm_(stm) {}
};

class Exp {
public:
  [[nodiscard]] virtual tree::Exp *UnEx() const = 0;
  [[nodiscard]] virtual tree::Stm *UnNx() const = 0;
  [[nodiscard]] virtual Cx UnCx(err::ErrorMsg *errormsg) const = 0;
};

class ExpAndTy {
public:
  tr::Exp *exp_;
  type::Ty *ty_;

  ExpAndTy(tr::Exp *exp, type::Ty *ty) : exp_(exp), ty_(ty) {}
};

class ExExp : public Exp {
public:
  tree::Exp *exp_;

  explicit ExExp(tree::Exp *exp) : exp_(exp) {}

  [[nodiscard]] tree::Exp *UnEx() const override { return exp_; }
  [[nodiscard]] tree::Stm *UnNx() const override {
    return new tree::ExpStm(exp_);
  }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) const override {
    tree::CjumpStm *stm = new tree::CjumpStm(
        tree::NE_OP, exp_, new tree::ConstExp(0), nullptr, nullptr);
    return tr::Cx(&stm->true_label_, &stm->false_label_, stm);
  }
};

class NxExp : public Exp {
public:
  tree::Stm *stm_;

  explicit NxExp(tree::Stm *stm) : stm_(stm) {}

  [[nodiscard]] tree::Exp *UnEx() const override {
    return new tree::EseqExp(stm_, new tree::ConstExp(0));
  }
  [[nodiscard]] tree::Stm *UnNx() const override { return stm_; }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) const override {
    assert(false);
  }
};

class CxExp : public Exp {
public:
  Cx cx_;

  CxExp(temp::Label **trues, temp::Label **falses, tree::Stm *stm)
      : cx_(trues, falses, stm) {}

  [[nodiscard]] tree::Exp *UnEx() const override {
    temp::Temp *result = temp::TempFactory::NewTemp();
    temp::Label *trues = temp::LabelFactory::NewLabel(),
                *falses = temp::LabelFactory::NewLabel();
    *cx_.trues_ = trues, *cx_.falses_ = falses;
    return new tree::EseqExp(
        new tree::MoveStm(new tree::TempExp(result), new tree::ConstExp(1)),
        new tree::EseqExp(
            cx_.stm_, new tree::EseqExp(
                          new tree::LabelStm(falses),
                          new tree::EseqExp(
                              new tree::MoveStm(new tree::TempExp(result),
                                                new tree::ConstExp(0)),
                              new tree::EseqExp(new tree::LabelStm(trues),
                                                new tree::TempExp(result))))));
  }
  [[nodiscard]] tree::Stm *UnNx() const override { return cx_.stm_; }
  [[nodiscard]] Cx UnCx(err::ErrorMsg *errormsg) const override { return cx_; }
};

void ProgTr::Translate() {
  FillBaseVEnv();
  FillBaseTEnv();
  main_level_ = Level::Outermost();
  temp::Label *mainLabel = temp::LabelFactory::NamedLabel("tigermain");
  tr::ExpAndTy *main = absyn_tree_.get()->Translate(
      venv_.get(), tenv_.get(), main_level_.get(), mainLabel, errormsg_.get());
  frags->PushBack(
      new frame::ProcFrag(main->exp_->UnNx(), main_level_.get()->frame_));
}

} // namespace tr

namespace absyn {

tr::ExpAndTy *AbsynTree::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  return root_->Translate(venv, tenv, level, label, errormsg);
}

tr::ExpAndTy *SimpleVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  // If variable exists
  env::EnvEntry *entry = venv->Look(sym_);
  type::Ty *ty = type::NilTy::Instance();
  if (!entry) {
    errormsg->Error(pos_, "undefined variable %s", sym_->Name().c_str());
    return new tr::ExpAndTy(VOID_EXP, type::NilTy::Instance());
  }
  if (typeid(*entry) != typeid(env::VarEntry)) {
    errormsg->Error(pos_, "variable required");
    return new tr::ExpAndTy(VOID_EXP, type::NilTy::Instance());
  }
  env::VarEntry *var = static_cast<env::VarEntry *>(entry);
  assert(var->access_);
  tree::Exp *exp = var->access_->ToExp(level);
  return new tr::ExpAndTy(new tr::ExExp(exp), var->ty_);
}

tr::ExpAndTy *FieldVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  // If variable is a record
  tr::ExpAndTy *var = var_->Translate(venv, tenv, level, label, errormsg);
  type::Ty *varType = var->ty_->ActualTy();
  if (typeid(*varType) != typeid(type::RecordTy)) {
    errormsg->Error(pos_, "not a record type");
    return new tr::ExpAndTy(VOID_EXP, type::NilTy::Instance());
  }

  type::RecordTy *record = static_cast<type::RecordTy *>(varType);
  // If corresponding field exists
  // The position of desired field in definition
  int order = 0;
  for (type::Field *field : record->fields_->GetList()) {
    if (field->name_->Name() == sym_->Name()) {
      // Found the desired field, return immediately
      tree::MemExp *exp = new tree::MemExp(new tree::BinopExp(
          tree::BinOp::PLUS_OP, var->exp_->UnEx(),
          new tree::ConstExp(order * reg_manager->WordSize())));
      return new tr::ExpAndTy(new tr::ExExp(exp), field->ty_);
    }
    ++order;
  }
  errormsg->Error(pos_, "field %s doesn't exist", sym_->Name().c_str());
  return new tr::ExpAndTy(VOID_EXP, type::NilTy::Instance());
}

tr::ExpAndTy *SubscriptVar::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                      tr::Level *level, temp::Label *label,
                                      err::ErrorMsg *errormsg) const {
  // If variable is an array
  tr::ExpAndTy *var = var_->Translate(venv, tenv, level, label, errormsg),
               *subscript =
                   subscript_->Translate(venv, tenv, level, label, errormsg);
  if (typeid(*var->ty_) != typeid(type::ArrayTy)) {
    errormsg->Error(pos_, "array type required");
    return new tr::ExpAndTy(VOID_EXP, type::NilTy::Instance());
  }

  tree::MemExp *exp = new tree::MemExp(new tree::BinopExp(
      tree::BinOp::PLUS_OP, var->exp_->UnEx(),
      new tree::BinopExp(tree::BinOp::MUL_OP, subscript->exp_->UnEx(),
                         new tree::ConstExp(reg_manager->WordSize()))));
  type::ArrayTy *array = static_cast<type::ArrayTy *>(var->ty_);
  return new tr::ExpAndTy(new tr::ExExp(exp), array->ty_);
}

tr::ExpAndTy *VarExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  return var_->Translate(venv, tenv, level, label, errormsg);
}

tr::ExpAndTy *NilExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(0)),
                          type::NilTy::Instance());
}

tr::ExpAndTy *IntExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  return new tr::ExpAndTy(new tr::ExExp(new tree::ConstExp(val_)),
                          type::IntTy::Instance());
}

tr::ExpAndTy *StringExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  temp::Label *stringLabel = temp::LabelFactory::NewLabel();
  frags->PushBack(new frame::StringFrag(stringLabel, str_));
  tree::Exp *exp = new tree::NameExp(stringLabel);
  return new tr::ExpAndTy(new tr::ExExp(exp), type::StringTy::Instance());
}

tr::ExpAndTy *CallExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *label,
                                 err::ErrorMsg *errormsg) const {
  // If function is defined
  env::EnvEntry *entry = venv->Look(func_);
  if (!entry) {
    errormsg->Error(pos_, "undefined function %s", func_->Name().c_str());
    return new tr::ExpAndTy(VOID_EXP, type::VoidTy::Instance());
  }
  // If the name is a function
  if (typeid(*entry) != typeid(env::FunEntry)) {
    errormsg->Error(pos_, "function required");
    return new tr::ExpAndTy(VOID_EXP, type::NilTy::Instance());
  }
  env::FunEntry *function = static_cast<env::FunEntry *>(entry);
  const std::list<type::Ty *> &formalTys = function->formals_->GetList();
  auto formalTy = formalTys.cbegin();
  std::list<Exp *> args = args_->GetList();
  // If numbers of actual and formal parameters match
  if (args.size() > formalTys.size()) {
    errormsg->Error(pos_ - 1, "too many params in function " + func_->Name());
    return new tr::ExpAndTy(VOID_EXP, type::VoidTy::Instance());
  }
  tree::ExpList *argList = new tree::ExpList();
  // If this is a user function
  if (function->level_) {
    // Pass static link as the first parameter
    argList->Append(
        static_cast<tree::MemExp *>(level->StaticLink(function->level_)));
  }
  // Check and build argument list
  for (Exp *arg : args) {
    tr::ExpAndTy *arg_ = arg->Translate(venv, tenv, level, label, errormsg);
    if (errormsg->AnyErrors()) {
      break;
    }
    // If types of actual and formal parameters match
    if (!(*formalTy)->IsSameType(arg_->ty_)) {
      errormsg->Error(arg->pos_, "para type mismatch");
      return new tr::ExpAndTy(VOID_EXP, type::VoidTy::Instance());
    }
    argList->Append(arg_->exp_->UnEx());
    ++formalTy;
  }
  tree::Exp *exp = new tree::CallExp(new tree::NameExp(func_), argList);
  return new tr::ExpAndTy(new tr::ExExp(exp), function->result_);
}

tr::ExpAndTy *OpExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *label,
                               err::ErrorMsg *errormsg) const {
  tr::ExpAndTy *left = left_->Translate(venv, tenv, level, label, errormsg),
               *right = right_->Translate(venv, tenv, level, label, errormsg);
  if (errormsg->AnyErrors()) {
    return left;
  }
  tr::Exp *exp = nullptr;
  // If operands of arithmetic operators are integers
  if (oper_ < EQ_OP) {
    if (!left->ty_->IsSameType(type::IntTy::Instance())) {
      errormsg->Error(left_->pos_, "integer required");
      return new tr::ExpAndTy(VOID_EXP, type::IntTy::Instance());
    }
    if (!right->ty_->IsSameType(type::IntTy::Instance())) {
      errormsg->Error(right_->pos_, "integer required");
      return new tr::ExpAndTy(VOID_EXP, type::IntTy::Instance());
    }

    tree::BinOp op = static_cast<tree::BinOp>(oper_);
    exp = new tr::ExExp(
        new tree::BinopExp(op, left->exp_->UnEx(), right->exp_->UnEx()));
  } else {
    // If operands types of logical operators match
    if (!left->ty_->IsSameType(right->ty_)) {
      errormsg->Error(pos_, "same type required");
      return left;
    }

    tree::CjumpStm *stm = nullptr;
    switch (oper_) {
    case absyn::Oper::EQ_OP:
    case absyn::Oper::NEQ_OP: {
      tree::RelOp op =
          oper_ == absyn::Oper::EQ_OP ? tree::RelOp::EQ_OP : tree::RelOp::NE_OP;
      if (left->ty_->IsSameType(type::StringTy::Instance())) {
        // call runtime-system function for string comparison
        tree::ExpList *args = new tree::ExpList();
        args->Append(left->exp_->UnEx());
        args->Append(right->exp_->UnEx());
        stm = new tree::CjumpStm(op, frame::ExternalCall("string_equal", args),
                                 new tree::ConstExp(1), nullptr, nullptr);
      } else {
        stm = new tree::CjumpStm(op, left->exp_->UnEx(), right->exp_->UnEx(),
                                 nullptr, nullptr);
      }
      break;
    }
    case absyn::Oper::LT_OP:
      stm = new tree::CjumpStm(tree::RelOp::LT_OP, left->exp_->UnEx(),
                               right->exp_->UnEx(), nullptr, nullptr);
      break;
    case absyn::Oper::LE_OP:
      stm = new tree::CjumpStm(tree::RelOp::LE_OP, left->exp_->UnEx(),
                               right->exp_->UnEx(), nullptr, nullptr);
      break;
    case absyn::Oper::GT_OP:
      stm = new tree::CjumpStm(tree::RelOp::GT_OP, left->exp_->UnEx(),
                               right->exp_->UnEx(), nullptr, nullptr);
      break;
    case absyn::Oper::GE_OP:
      stm = new tree::CjumpStm(tree::RelOp::GE_OP, left->exp_->UnEx(),
                               right->exp_->UnEx(), nullptr, nullptr);
      break;
    }
    exp = new tr::CxExp(&stm->true_label_, &stm->false_label_, stm);
  }
  return new tr::ExpAndTy(exp, type::IntTy::Instance());
}

tr::ExpAndTy *RecordExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  // If corresponding record type is defined
  type::Ty *type = tenv->Look(typ_);
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", typ_->Name().c_str());
    return new tr::ExpAndTy(VOID_EXP, type::NilTy::Instance());
  }
  // If corresponding type is record
  type = type->ActualTy();
  if (typeid(*type) != typeid(type::RecordTy)) {
    errormsg->Error(pos_, "record type required");
    return new tr::ExpAndTy(VOID_EXP, type::NilTy::Instance());
  }
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", typ_->Name().c_str());
    return new tr::ExpAndTy(VOID_EXP, type::NilTy::Instance());
  }
  type::RecordTy *recordTy = static_cast<type::RecordTy *>(type);

  // Check field types and build initialization statements
  type::FieldList *fieldList = new type::FieldList();
  auto recordTys = recordTy->fields_->GetList();
  auto it = recordTys.cbegin();
  int order = 0;
  // Register of pointer to the start of record
  temp::Temp *record = temp::TempFactory::NewTemp();
  tree::ExpList *mallocArgs = new tree::ExpList();
  mallocArgs->Append(
      new tree::ConstExp(recordTys.size() * reg_manager->WordSize()));
  tree::SeqStm *initStm = new tree::SeqStm(
                   new tree::MoveStm(
                       new tree::TempExp(record),
                       frame::ExternalCall("alloc_record", mallocArgs)),
                   nullptr),
               *tail = initStm;
  for (absyn::EField *efield : fields_->GetList()) {
    tr::ExpAndTy *field =
        efield->exp_->Translate(venv, tenv, level, label, errormsg);
    fieldList->Append(new type::Field(efield->name_, field->ty_));

    // If fields number in expression is larger than that in type definition
    if (it == recordTys.cend()) {
      errormsg->Error(pos_, "too many fields");
    }
    // If the order of fields in expression is same as that in type definition
    if (efield->name_->Name() != (*it)->name_->Name()) {
      errormsg->Error(pos_, "field %s doesn't exist",
                      efield->name_->Name().c_str());
    }
    // If the type of fields in expression is same as that in type definition
    if (!field->ty_->IsSameType((*it)->ty_)) {
      errormsg->Error(pos_, "type of field %s doesn't match",
                      efield->name_->Name().c_str());
    }

    // Append statement to initialize the field
    tree::MemExp *mem = new tree::MemExp(new tree::BinopExp(
        tree::BinOp::PLUS_OP, new tree::TempExp(record),
        new tree::ConstExp(order * reg_manager->WordSize())));
    tail->right_ =
        new tree::SeqStm(new tree::MoveStm(mem, field->exp_->UnEx()), nullptr);
    tail = static_cast<tree::SeqStm *>(tail->right_);
    ++it, ++order;
  }
  // Avoid Segmentation Fault during canonicalizing
  tail->right_ = VOID_STM;
  // Handle "{}"
  if (fieldList->GetList().empty()) {
    type = type::NilTy::Instance();
  }
  tree::EseqExp *exp = new tree::EseqExp(initStm, new tree::TempExp(record));
  return new tr::ExpAndTy(new tr::ExExp(exp), type);
}

tr::ExpAndTy *SeqExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  tree::SeqStm *stms = nullptr, *tail = nullptr;
  auto expList = seq_->GetList();
  // Pop last expression for later evaluation
  absyn::Exp *lastExp = expList.back();
  expList.pop_back();
  for (auto exp = expList.cbegin(); exp != expList.cend(); ++exp) {
    tr::ExpAndTy *exp_ = (*exp)->Translate(venv, tenv, level, label, errormsg);
    tree::Stm *expStm = exp_->exp_->UnNx();
    if (exp == expList.cbegin()) {
      stms = tail = new tree::SeqStm(expStm, nullptr);
    } else {
      tail->right_ = new tree::SeqStm(expStm, nullptr);
      tail = static_cast<tree::SeqStm *>(tail->right_);
    }
  }
  // The result and type of SeqExp are determined by the last expression
  tr::ExpAndTy *seqExpResult =
      lastExp->Translate(venv, tenv, level, label, errormsg);
  tree::Exp *exp = nullptr;
  if (stms) {
    // Avoid Segmentation Fault during canonicalizing
    tail->right_ = VOID_STM;
    exp = new tree::EseqExp(stms, seqExpResult->exp_->UnEx());
  } else {
    exp = seqExpResult->exp_->UnEx();
  }
  return new tr::ExpAndTy(new tr::ExExp(exp), seqExpResult->ty_);
}

tr::ExpAndTy *AssignExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                   tr::Level *level, temp::Label *label,
                                   err::ErrorMsg *errormsg) const {
  tr::ExpAndTy *var = var_->Translate(venv, tenv, level, label, errormsg),
               *exp = exp_->Translate(venv, tenv, level, label, errormsg);
  // If value type matches variable type
  if (!var->ty_->IsSameType(exp->ty_)) {
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
  tree::MoveStm *stm = new tree::MoveStm(var->exp_->UnEx(), exp->exp_->UnEx());
  return new tr::ExpAndTy(new tr::NxExp(stm), type::VoidTy::Instance());
}

tr::ExpAndTy *IfExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                               tr::Level *level, temp::Label *label,
                               err::ErrorMsg *errormsg) const {
  tr::ExpAndTy *test = test_->Translate(venv, tenv, level, label, errormsg),
               *then = then_->Translate(venv, tenv, level, label, errormsg);
  if (errormsg->AnyErrors()) {
    return new tr::ExpAndTy(VOID_EXP, then->ty_);
  }

  temp::Label *trues = temp::LabelFactory::NewLabel(),
              *falses = temp::LabelFactory::NewLabel(),
              *join = temp::LabelFactory::NewLabel();
  tr::Cx cx = test->exp_->UnCx(errormsg);
  *cx.trues_ = trues, *cx.falses_ = falses;
  temp::Temp *result = temp::TempFactory::NewTemp();
  tree::CjumpStm *testStm = static_cast<tree::CjumpStm *>(cx.stm_);

  tr::Exp *exp = nullptr;
  if (elsee_) {
    tr::ExpAndTy *elsee = elsee_->Translate(venv, tenv, level, label, errormsg);
    // If types of then expression and else expression match
    if (!then->ty_->IsSameType(elsee->ty_)) {
      errormsg->Error(pos_ - 1, "then exp and else exp type mismatch");
      return new tr::ExpAndTy(VOID_EXP, then->ty_);
    }
    std::vector<temp::Label *> *jumps = new std::vector<temp::Label *>{join};
    tree::SeqStm *trueBranch = new tree::SeqStm(
                     new tree::LabelStm(trues),
                     new tree::SeqStm(
                         new tree::MoveStm(new tree::TempExp(result),
                                           then->exp_->UnEx()),
                         new tree::JumpStm(new tree::NameExp(join), jumps))),
                 *falseBranch = new tree::SeqStm(
                     new tree::LabelStm(falses),
                     new tree::SeqStm(
                         new tree::MoveStm(new tree::TempExp(result),
                                           elsee->exp_->UnEx()),
                         new tree::JumpStm(new tree::NameExp(join), jumps)));
    exp = new tr::ExExp(new tree::EseqExp(
        new tree::SeqStm(
            testStm,
            new tree::SeqStm(
                trueBranch,
                new tree::SeqStm(falseBranch, new tree::LabelStm(join)))),
        new tree::TempExp(result)));
  } else {
    // If the body of if-then expression produces no value
    if (!errormsg->AnyErrors() &&
        !then->ty_->IsSameType(type::VoidTy::Instance())) {
      errormsg->Error(then_->pos_, "if-then exp's body must produce no value");
      return new tr::ExpAndTy(VOID_EXP, then->ty_);
    }
    exp = new tr::NxExp(new tree::SeqStm(
        testStm,
        new tree::SeqStm(
            new tree::LabelStm(trues),
            new tree::SeqStm(then->exp_->UnNx(), new tree::LabelStm(falses)))));
  }
  return new tr::ExpAndTy(exp, then->ty_);
}

tr::ExpAndTy *WhileExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  temp::Label *testLabel = temp::LabelFactory::NewLabel(),
              *bodyLabel = temp::LabelFactory::NewLabel(),
              *doneLabel = temp::LabelFactory::NewLabel();
  // If the body of while expression produces no value
  tr::ExpAndTy *test = test_->Translate(venv, tenv, level, label, errormsg),
               // Pass done label to body for BreakExp
      *body = body_->Translate(venv, tenv, level, doneLabel, errormsg);
  if (!body->ty_->IsSameType(type::VoidTy::Instance())) {
    errormsg->Error(pos_, "while body must produce no value");
  }
  tr::Cx cx = test->exp_->UnCx(errormsg);
  *cx.trues_ = bodyLabel, *cx.falses_ = doneLabel;
  tree::Stm *testStm = cx.stm_;
  tree::SeqStm *bodyStm = new tree::SeqStm(
      body->exp_->UnNx(),
      new tree::SeqStm(
          new tree::JumpStm(new tree::NameExp(testLabel),
                            new std::vector<temp::Label *>{testLabel}),
          new tree::LabelStm(doneLabel)));
  tree::SeqStm *stm = new tree::SeqStm(
      new tree::LabelStm(testLabel),
      new tree::SeqStm(
          testStm, new tree::SeqStm(new tree::LabelStm(bodyLabel), bodyStm)));
  return new tr::ExpAndTy(new tr::NxExp(stm), type::VoidTy::Instance());
}

tr::ExpAndTy *ForExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  venv->Enter(var_, new env::VarEntry(tr::Access::AllocLocal(level, false),
                                      type::IntTy::Instance(), true));
  tr::ExpAndTy *lo = lo_->Translate(venv, tenv, level, label, errormsg),
               *hi = hi_->Translate(venv, tenv, level, label, errormsg);
  // If range type of for expression is integer
  if (!lo->ty_->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(lo_->pos_, "for exp's range type is not integer");
  }
  if (!hi->ty_->IsSameType(type::IntTy::Instance())) {
    errormsg->Error(hi_->pos_, "for exp's range type is not integer");
  }
  tr::ExpAndTy *body = body_->Translate(venv, tenv, level, label + 1, errormsg);
  venv->EndScope();

  int pos = lo_->pos_;
  absyn::VarDec *loopVar =
      new absyn::VarDec(pos, var_, sym::Symbol::UniqueSymbol("int"), lo_);
  absyn::VarDec *limitVar =
      new absyn::VarDec(pos, sym::Symbol::UniqueSymbol("limit"),
                        sym::Symbol::UniqueSymbol("int"), hi_);
  absyn::DecList *decList = new absyn::DecList(limitVar);
  decList->Prepend(loopVar);

  absyn::OpExp *loopCondition = new absyn::OpExp(
      pos, absyn::LE_OP,
      new absyn::VarExp(pos, new absyn::SimpleVar(pos, var_)),
      new absyn::VarExp(pos, new absyn::SimpleVar(pos, limitVar->var_)));
  absyn::ExpList *expList = new absyn::ExpList(new absyn::AssignExp(
      pos, new absyn::SimpleVar(pos, var_),
      new absyn::OpExp(pos, absyn::PLUS_OP,
                       new absyn::VarExp(pos, new absyn::SimpleVar(pos, var_)),
                       new absyn::IntExp(pos, 1))));
  expList->Prepend(body_);
  absyn::SeqExp *loopBody = new absyn::SeqExp(pos, expList);
  absyn::WhileExp *whileLoop =
      new absyn::WhileExp(pos, loopCondition, loopBody);

  absyn::LetExp *exp = new absyn::LetExp(pos, decList, whileLoop);
  return new tr::ExpAndTy(
      exp->Translate(venv, tenv, level, label, errormsg)->exp_,
      type::VoidTy::Instance());
}

tr::ExpAndTy *BreakExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  tree::JumpStm *stm = new tree::JumpStm(new tree::NameExp(label),
                                         new std::vector<temp::Label *>{label});
  return new tr::ExpAndTy(new tr::NxExp(stm), type::VoidTy::Instance());
}

tr::ExpAndTy *LetExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  venv->BeginScope();
  tenv->BeginScope();
  tree::SeqStm *stm = nullptr, *tail = nullptr;
  for (absyn::Dec *dec : decs_->GetList()) {
    tree::Stm *decStm =
        dec->Translate(venv, tenv, level, label, errormsg)->UnNx();
    if (!tail) {
      stm = tail = new tree::SeqStm(decStm, nullptr);
    } else {
      tail->right_ = new tree::SeqStm(decStm, nullptr);
      tail = static_cast<tree::SeqStm *>(tail->right_);
    }
  }
  // Avoid Segmentation Fault in canonicalizing
  tail->right_ = VOID_STM;
  tr::ExpAndTy *body = body_->Translate(venv, tenv, level, label, errormsg);
  tenv->EndScope();
  venv->EndScope();
  tree::Exp *exp = nullptr;
  if (stm) {
    exp = new tree::EseqExp(stm, body->exp_->UnEx());
  } else {
    exp = body->exp_->UnEx();
  }
  return new tr::ExpAndTy(new tr::ExExp(exp), body->ty_);
}

tr::ExpAndTy *ArrayExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                  tr::Level *level, temp::Label *label,
                                  err::ErrorMsg *errormsg) const {
  // If corresponding array type is defined
  type::Ty *type = tenv->Look(typ_);
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", typ_);
    return new tr::ExpAndTy(VOID_EXP, type::VoidTy::Instance());
  }
  // If corresponding type is array
  type = type->ActualTy();
  if (typeid(*type) != typeid(type::ArrayTy)) {
    errormsg->Error(pos_, "array type required");
    return new tr::ExpAndTy(VOID_EXP, type::VoidTy::Instance());
  }
  // If the type of elements in expression is same as that in type definition
  type::Ty *arrayTy = static_cast<type::ArrayTy *>(type)->ty_;
  tr::ExpAndTy *init = init_->Translate(venv, tenv, level, label, errormsg);
  if (!arrayTy->IsSameType(init->ty_)) {
    errormsg->Error(pos_, "type mismatch");
    return init;
  }
  tr::ExpAndTy *size = size_->Translate(venv, tenv, level, label, errormsg);

  tree::ExpList *args = new tree::ExpList();
  args->Append(size->exp_->UnEx());
  args->Append(init->exp_->UnEx());
  temp::Temp *array = temp::TempFactory::NewTemp();
  tree::EseqExp *exp = new tree::EseqExp(
      new tree::MoveStm(new tree::TempExp(array),
                        frame::ExternalCall("init_array", args)),
      new tree::TempExp(array));
  return new tr::ExpAndTy(new tr::ExExp(exp), type);
}

tr::ExpAndTy *VoidExp::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                 tr::Level *level, temp::Label *label,
                                 err::ErrorMsg *errormsg) const {
  return new tr::ExpAndTy(VOID_EXP, type::VoidTy::Instance());
}

tr::Exp *FunctionDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                                tr::Level *level, temp::Label *label,
                                err::ErrorMsg *errormsg) const {
  for (absyn::FunDec *function : functions_->GetList()) {
    temp::Label *functionLabel =
        temp::LabelFactory::NamedLabel(function->name_->Name());
    type::TyList *formalTys =
        function->params_->MakeFormalTyList(tenv, errormsg);
    type::Ty *resultTy = function->result_ ? tenv->Look(function->result_)
                                           : type::VoidTy::Instance();
    // If two functions have the same name, check only in this declaration list
    // not in environment, since we allow function shadowing
    for (absyn::FunDec *anotherFunction : functions_->GetList()) {
      if (function != anotherFunction &&
          function->name_ == anotherFunction->name_) {
        errormsg->Error(function->pos_, "two functions have the same name");
        return new tr::ExExp(new tree::ConstExp(0));
      }
    }
    venv->Enter(function->name_,
                new env::FunEntry(level, functionLabel, formalTys, resultTy));
  }
  for (absyn::FunDec *function : functions_->GetList()) {
    // Obtain entry entered in the first loop, along with function label & level
    env::FunEntry *entry =
        static_cast<env::FunEntry *>(venv->Look(function->name_));
    assert(entry);
    // Build escape list of function formal parameters,
    // static link automatically prepended when creating new level
    std::unique_ptr<BoolList> escapes = std::make_unique<BoolList>();
    for (const Field *field : function->params_->GetList()) {
      escapes->push_back(field->escape_);
    }
    // Level inside the function
    tr::Level *functionLevel =
        new tr::Level(level, entry->label_, std::move(escapes));

    type::TyList *formalTys =
        function->params_->MakeFormalTyList(tenv, errormsg);
    type::Ty *resultTy = function->result_ ? tenv->Look(function->result_)
                                           : type::VoidTy::Instance();
    venv->BeginScope();
    // Append formal args to venv
    tr::AccessList *accessList = functionLevel->Formals();
    auto ty = formalTys->GetList().cbegin();
    auto access = accessList->cbegin();
    for (absyn::Field *param : function->params_->GetList()) {
      venv->Enter(param->name_, new env::VarEntry(*access, *ty));
      ++ty;
      ++access;
    }
    tr::ExpAndTy *return_ = function->body_->Translate(
        venv, tenv, functionLevel, entry->label_, errormsg);
    type::Ty *returnTy = return_->ty_;
    // If void function returns a value
    if (resultTy->IsSameType(type::VoidTy::Instance()) &&
        !returnTy->IsSameType(type::VoidTy::Instance())) {
      errormsg->Error(pos_, "procedure returns value");
    }
    venv->EndScope();
    tree::MoveStm *stm = new tree::MoveStm(
        new tree::TempExp(reg_manager->ReturnValue()), return_->exp_->UnEx());
    frags->PushBack(
        new frame::ProcFrag(frame::ProcEntryExit1(functionLevel->frame_, stm),
                            functionLevel->frame_));
  }
  return VOID_EXP;
}

tr::Exp *VarDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                           tr::Level *level, temp::Label *label,
                           err::ErrorMsg *errormsg) const {
  assert(init_);
  type::Ty *type = nullptr;
  tr::ExpAndTy *init = init_->Translate(venv, tenv, level, label, errormsg);
  if (typ_) { // Explicit type
    type = tenv->Look(typ_);
    // If type is defined
    if (!type) {
      errormsg->Error(pos_, "undefined type %s", typ_->Name().c_str());
      return new tr::NxExp(VOID_STM);
    }
    // If value type matches specified type
    if (!type->IsSameType(init->ty_)) {
      errormsg->Error(init_->pos_, "type mismatch");
    }
  } else { // Implicit type with initializer
    type = init->ty_->ActualTy();
    // If Nil value is used to initialize an implicit-type variable
    if (type->IsSameType(type::NilTy::Instance()) &&
        typeid(*type) != typeid(type::RecordTy)) {
      errormsg->Error(pos_, "init should not be nil without type specified");
      return new tr::NxExp(VOID_STM);
    }
  }
  tr::Access *access = tr::Access::AllocLocal(level, escape_);
  venv->Enter(var_, new env::VarEntry(access, type));
  return new tr::NxExp(
      new tree::MoveStm(access->ToExp(level), init->exp_->UnEx()));
}

tr::Exp *TypeDec::Translate(env::VEnvPtr venv, env::TEnvPtr tenv,
                            tr::Level *level, temp::Label *label,
                            err::ErrorMsg *errormsg) const {
  for (NameAndTy *type : types_->GetList()) {
    // If two types have the same name, check only in this declaration list
    // not in environment, since we allow type shadowing
    for (NameAndTy *anotherType : types_->GetList()) {
      if (type != anotherType && type->name_ == anotherType->name_) {
        errormsg->Error(pos_, "two types have the same name");
        return VOID_EXP;
      }
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
        return VOID_EXP;
      }
      map.insert(std::make_pair(current, order));
      ++order;
      current = next;
    }
  }
  return VOID_EXP;
}

type::Ty *NameTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  return tenv->Look(name_);
}

type::Ty *RecordTy::Translate(env::TEnvPtr tenv,
                              err::ErrorMsg *errormsg) const {
  return new type::RecordTy(record_->MakeFieldList(tenv, errormsg));
}

type::Ty *ArrayTy::Translate(env::TEnvPtr tenv, err::ErrorMsg *errormsg) const {
  type::Ty *type = tenv->Look(array_);
  if (!type) {
    errormsg->Error(pos_, "undefined type %s", array_->Name().c_str());
    return type::NilTy::Instance();
  }
  return new type::ArrayTy(type);
}

} // namespace absyn
