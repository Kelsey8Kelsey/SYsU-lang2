#include "ConstantPropagation.hpp"

using namespace llvm;

PreservedAnalyses
ConstantPropagation::run(Module& mod, ModuleAnalysisManager& mam)
{
  int constPropagateTimes = 0;
  std::unordered_map<llvm::GlobalVariable *, bool> isWrote;

  // 1. 标记被写入的全局变量
  for (auto& func : mod) {
    // 遍历每个函数的基本块
    for (auto& bb : func) {
      // 遍历每个基本块的指令
      for (auto& inst : bb) {
        if (StoreInst *storeInst = dyn_cast<StoreInst>(&inst)) {
          // Value *StoreInst::getPointerOperand() { return getOperand(1); }

          Value *ptrOperand = storeInst->getPointerOperand();
          if (GlobalVariable *GV = dyn_cast<GlobalVariable>(ptrOperand)) {
            isWrote[GV] = true;
          }
        }
      }
    }
  }

  std::vector<Instruction*> instToErase;
  // 2. 替换未被写入的全局变量
  for (GlobalVariable &GV : mod.globals())
  {
    if (GV.isConstant() || isWrote[&GV]) continue;
    else if (GV.getValueType()->isArrayTy()) {
      // TODO
      continue;
    }
    else {
      if (GV.hasInitializer())  // 如果全局变量有初始值
      {
        Constant* init = GV.getInitializer();
        for (Use &use : GV.uses()) {
          User *user = use.getUser();
          if (LoadInst *loadInst = dyn_cast<LoadInst>(user)) {  // 如果使用者是Load指令
            loadInst->replaceAllUsesWith(init);
            instToErase.push_back(loadInst);
            ++constPropagateTimes;
          } else if (Instruction *inst = dyn_cast<Instruction>(user)) {   // 如果使用者是其他指令
            inst->replaceUsesOfWith(&GV, init);
            ++constPropagateTimes;
          }
        }
      }
    }
  }

  // 统一删除被折叠为常量的指令
      for (auto& i : instToErase)
        i->eraseFromParent();

  mOut << "ConstantPropagation running...\nTo propagate " << constPropagateTimes
       << " instructions\n";
  return PreservedAnalyses::all();
}