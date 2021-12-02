#ifndef TIGER_TRANSLATE_TRANSLATE_H_
#define TIGER_TRANSLATE_TRANSLATE_H_

#include <list>
#include <memory>

#include "tiger/absyn/absyn.h"
#include "tiger/env/env.h"
#include "tiger/errormsg/errormsg.h"
#include "tiger/frame/frame.h"
#include "tiger/semant/types.h"

namespace tr {

// For void and error return type
#define VOID_EXP (new tr::ExExp(new tree::ConstExp(0)))
#define VOID_STM (new tree::ExpStm(new tree::ConstExp(0)))

typedef std::list<Access *> AccessList;

class Exp;
class ExpAndTy;
class Level;

class Access {
public:
  Level *level_;
  frame::Access *access_;

  Access(Level *level, frame::Access *access)
      : level_(level), access_(access) {}
  static Access *AllocLocal(Level *level, bool escape);
  /*
   * Get the expression to access the variable, with static links taken into
   * account
   */
  tree::Exp *ToExp(Level *currentLevel);
};

class Level {
public:
  frame::Frame *frame_;
  Level *parent_;

  Level(Level *parent, temp::Label *name, std::unique_ptr<BoolList> escapes);
  /*
   * Get the access list of formal parameters with static links taken into
   * account
   */
  AccessList *Formals();
  static std::unique_ptr<Level> Outermost();
  // Get the static link from current level to the target level
  tree::Exp *StaticLink(Level *targetLevel);
};

class ProgTr {
public:
  ProgTr(std::unique_ptr<absyn::AbsynTree> absyn_tree,
         std::unique_ptr<err::ErrorMsg> errormsg)
      : absyn_tree_(std::move(absyn_tree)), errormsg_(std::move(errormsg)),
        tenv_(std::make_unique<env::TEnv>()),
        venv_(std::make_unique<env::VEnv>()){};

  /**
   * Translate IR tree
   */
  void Translate();

  /**
   * Transfer the ownership of errormsg to outer scope
   * @return unique pointer to errormsg
   */
  std::unique_ptr<err::ErrorMsg> TransferErrormsg() {
    return std::move(errormsg_);
  }

private:
  std::unique_ptr<absyn::AbsynTree> absyn_tree_;
  std::unique_ptr<err::ErrorMsg> errormsg_;
  std::unique_ptr<Level> main_level_;
  std::unique_ptr<env::TEnv> tenv_;
  std::unique_ptr<env::VEnv> venv_;

  // Fill base symbol for var env and type env
  void FillBaseVEnv();
  void FillBaseTEnv();
};

} // namespace tr

#endif
