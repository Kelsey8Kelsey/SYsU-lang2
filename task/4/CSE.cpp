#include "CSE.hpp"

using namespace llvm;

PreservedAnalyses
CSE::run(Module& mod, ModuleAnalysisManager& mam)
{
    int CSECounts = 0;

    for (auto& func : mod) {
        for (auto& bb : func) {
            std::vector<Instruction*> instToErase;
            std::vector<BasicBlock*> succBlocks {&bb};

            for (auto instAIter = bb.begin(); instAIter != bb.end(); ++instAIter) {
                auto& instA = *instAIter;

                // 判断当前指令是否是二元运算指令
                if (auto temp = dyn_cast<BinaryOperator>(&instA)) {}
                else continue;

                // 如果指令已经在删除列表中，跳过
                if (std::find(instToErase.begin(), instToErase.end(), &instA) != instToErase.end()) continue;

                // 将基本块的后继块加入 succBlocks 向量中
                for (auto succBBIter = succ_begin(&bb); succBBIter != succ_end(&bb); ++succBBIter) {
                    succBlocks.emplace_back(*succBBIter);
                }

                // 遍历当前基本块及其后继块
                for (auto succBB:succBlocks) {

                    BasicBlock::iterator instBIterBegin;
                    BasicBlock::iterator instBIterEnd;

                    // 如果当前块与后继块相同，设置起始和结束迭代器
                    if (&bb == succBB) {
                        instBIterBegin = std::next(instAIter);
                        instBIterEnd = bb.end();
                    } else {
                        instBIterBegin = succBB->begin();
                        instBIterEnd = succBB->end();
                    }

                    auto instBIter = instBIterBegin;
                    int i = 0;      // 限制遍历深度，防止无限循环
                    while(true) {
                        if (instBIter == instBIterEnd || i>=100) break;
                        auto& instB = *instBIter;
                        if (std::find(instToErase.begin(), instToErase.end(), &instB) != instToErase.end()) continue;

                        auto OpA = instA.getOpcode();
                        auto OpB = instB.getOpcode();

                        // 检查两个指令的操作码是否相同
                        if (OpA == OpB) {
                            auto binOpA = dyn_cast<BinaryOperator>(&instA);
                            auto binOpB = dyn_cast<BinaryOperator>(&instB);
                            if (binOpA && binOpB) {
                                // 检查操作数是否相同
                                if (binOpA->getOperand(0)==binOpB->getOperand(0) && binOpA->getOperand(1)==binOpB->getOperand(1)) {
                                    binOpB->replaceAllUsesWith(binOpA);      // 用 binOpA 替换 binOpB 的所有使用
                                    instToErase.push_back(binOpB);           // 标记 binOpB 为删除
                                    CSECounts++;
                                }
                            }
                        }
                        
                        ++instBIter;
                        ++i;
                    }
                }
            }

            // 统一删除标记的指令
            for (auto& i : instToErase)
                i->eraseFromParent();

        }
    }

    mOut << "CSE running...\nTo eliminate " << CSECounts << " instructions\n";
    
    return PreservedAnalyses::all();
}