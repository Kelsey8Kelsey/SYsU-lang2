#include "DCE.hpp"

using namespace llvm;

PreservedAnalyses
DCE::run(Module& mod, ModuleAnalysisManager& mam) {
  bool change = false;

  for (auto& func : mod) {
    for (auto& bb : func) {
      std::vector<Instruction*> instToErase;
      for (auto& inst : bb) {
        if (isInstructionTriviallyDead(&inst)) {
          instToErase.push_back(&inst);
        }
      }
      for (auto& i : instToErase) {
        if (!i->use_empty()) continue;
        i->eraseFromParent();
        change = true;
      }
    }
  }

  return change ? PreservedAnalyses::none() : PreservedAnalyses::all();
}