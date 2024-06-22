#include "Combine.hpp"

using namespace llvm;

PreservedAnalyses
Combine::run(Module& mod, ModuleAnalysisManager& mam) {

    for (auto& func : mod) {
        for (auto& bb : func) {
            std::vector<Instruction*> instToErase;
            for (auto instIter = bb.begin(); instIter != bb.end(); ++instIter) {
                auto& inst = *instIter;
                // 如果指令已经在删除列表中，跳过
                if (std::find(instToErase.begin(), instToErase.end(), &inst) != instToErase.end()) continue;
                // 确保当前指令不是基本块中的最后一条指令
                if (instIter != bb.end()) {
                    // 判断当前指令是否是二元运算指令
                    if (auto binOp = dyn_cast<BinaryOperator>(&inst)) {
                        if (binOp->hasOneUse()) {   // 如果当前指令只有一个user
                            if (auto user = dyn_cast<BinaryOperator>(*binOp->user_begin())) {   // 获取用户指令
                                // 如果user指令是加法指令并且它的第一个操作数是当前指令
                                if (user->getOpcode() == Instruction::Add && user->getOperand(0)==binOp) {
                                    auto constVal = dyn_cast<ConstantInt>(binOp->getOperand(1));
                                    auto userConstVal = dyn_cast<ConstantInt>(user->getOperand(1));
                                    // 如果二元运算指令和用户指令的第二个operand都是常量
                                    if (constVal && userConstVal) {
                                        // 计算新的常量值
                                        auto combinedConst = ConstantInt::get(constVal->getType(), constVal->getSExtValue()+userConstVal->getSExtValue());
                                        // 创建新的加法指令
                                        auto newBinInst = BinaryOperator::Create(Instruction::Add, binOp->getOperand(0), combinedConst);
                                        user->replaceAllUsesWith(newBinInst);   // 将user的所有use替换为新的加法指令
                                        newBinInst->insertAfter(user);
                                        instToErase.push_back(&inst);
                                        instToErase.push_back(user);
                                    }
                                }
                            }
                        }
                    }

                }
            }
            for (auto& i : instToErase) {
                i->eraseFromParent();
            }    
        }
    }

    return PreservedAnalyses::all();
}