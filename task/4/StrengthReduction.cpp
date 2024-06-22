#include "StrengthReduction.hpp"

using namespace llvm;

PreservedAnalyses
StrengthReduction::run(Module& mod, ModuleAnalysisManager& mam)
{
  int strengthReductionTimes = 0;

  // 遍历所有函数
  for (auto& func : mod) {
    // 遍历每个函数的基本块
    for (auto& bb : func) {
      std::vector<Instruction*> instToErase;
      // 遍历每个基本块的指令

      for (auto instIter = bb.begin(); instIter != bb.end(); ++instIter) {
        auto& inst = *instIter;

        if (auto binOp = dyn_cast<BinaryOperator>(&inst)) {   // 如果指令是二元运算
          Value* lhs = binOp->getOperand(0);
          Value* rhs = binOp->getOperand(1);
          auto constLhs = dyn_cast<ConstantInt>(lhs);
          auto constRhs = dyn_cast<ConstantInt>(rhs);

          if (binOp->getOpcode() == Instruction::Mul) {
            if (constLhs && constLhs->getSExtValue()==0) {
              binOp->replaceAllUsesWith(ConstantInt::get(lhs->getType(), 0));
              instToErase.push_back(binOp);
              ++strengthReductionTimes;

            } else if (constRhs && constRhs->getSExtValue()==0) {
              binOp->replaceAllUsesWith(ConstantInt::get(rhs->getType(), 0));
              instToErase.push_back(binOp);
              ++strengthReductionTimes;

            } else if (constLhs && constLhs->getSExtValue()==1) {
              binOp->replaceAllUsesWith(rhs);
              instToErase.push_back(binOp);
              ++strengthReductionTimes;

            } else if (constRhs && constRhs->getSExtValue()==1) {
              binOp->replaceAllUsesWith(lhs);
              instToErase.push_back(binOp);
              ++strengthReductionTimes;

            } else if (constLhs && rhs->getType()->isIntegerTy()) {
              auto intVal = constLhs->getSExtValue();
              if (intVal>0 && (intVal & (intVal - 1))==0) {
                auto shamt = ConstantInt::get(lhs->getType(), static_cast<uint64_t>(std::log2((double)intVal)));
                llvm::IRBuilder<> TheBuilder(&inst);
                auto newInst = TheBuilder.CreateShl(rhs, shamt, "strengthReduction");
                binOp->replaceAllUsesWith(newInst);
                instToErase.push_back(binOp);
                ++strengthReductionTimes;
              }

            } else if (constRhs && lhs->getType()->isIntegerTy()) {
              auto intVal = constRhs->getSExtValue();
              auto nextInstIter = std::next(instIter);
              if (nextInstIter!=bb.end()) {   // a * b / b 的情况
                auto& nextInst = *nextInstIter;
                if (auto nextBinOp = dyn_cast<BinaryOperator>(&nextInst)) {
                  if ((nextBinOp->getOpcode() == Instruction::SDiv || nextBinOp->getOpcode() == Instruction::UDiv) && nextBinOp->getOperand(1) == rhs) {
                    binOp->replaceAllUsesWith(lhs);
                    instToErase.push_back(binOp);
                    instToErase.push_back(nextBinOp);
                    strengthReductionTimes += 2;
                    ++instIter;
                    continue;
                  }
                }
              }
              if (intVal>0 && (intVal & (intVal - 1))==0) {   // 若intVal大于0且是2的幂
                auto shamt = ConstantInt::get(rhs->getType(), static_cast<uint64_t>(std::log2((double)intVal)));
                llvm::IRBuilder<> TheBuilder(&inst);
                auto newInst = TheBuilder.CreateShl(lhs, shamt, "strengthReduction");
                binOp->replaceAllUsesWith(newInst);
                instToErase.push_back(binOp);
                ++strengthReductionTimes;
              }
            }
          } else if (binOp->getOpcode() == Instruction::UDiv || binOp->getOpcode() == Instruction::SDiv) {
            
            if (constLhs && constLhs->getSExtValue()==0) {
              binOp->replaceAllUsesWith(ConstantInt::get(lhs->getType(), 0));
              instToErase.push_back(binOp);
              ++strengthReductionTimes;

            } else if (constRhs && constRhs->getSExtValue()==1) {
              binOp->replaceAllUsesWith(lhs);
              instToErase.push_back(binOp);
              ++strengthReductionTimes;

            }
            // else if (constRhs && rhs->getType()->isIntegerTy()) {
            //   auto intVal = constRhs->getSExtValue();
            //   if ( intVal>0 && (intVal & (intVal - 1))==0) {  // Only workable when " lhs >=0 "
            //     auto shamt = ConstantInt::get(rhs->getType(), static_cast<uint64_t>(std::log2((double)intVal)));
            //     llvm::IRBuilder<> TheBuilder(&inst);
            //     auto newInst = TheBuilder.CreateAShr(lhs, shamt, "strengthReduction");
            //     binOp->replaceAllUsesWith(newInst);
            //     instToErase.push_back(binOp);
            //     ++strengthReductionTimes;
            //   }
            // }
            else if (constLhs && lhs->getType()->isIntegerTy() && constRhs && rhs->getType()->isIntegerTy()) {
              auto intValLhs = constLhs->getSExtValue();
              auto intVal = constRhs->getSExtValue();
              if ( intValLhs>=0 && intVal>0 && (intVal & (intVal - 1))==0) {
                auto shamt = ConstantInt::get(rhs->getType(), static_cast<uint64_t>(std::log2((double)intVal)));
                llvm::IRBuilder<> TheBuilder(&inst);
                auto newInst = TheBuilder.CreateAShr(lhs, shamt, "strengthReduction");
                binOp->replaceAllUsesWith(newInst);
                instToErase.push_back(binOp);
                ++strengthReductionTimes;
              }
            }
          } else if (binOp->getOpcode() == Instruction::URem || binOp->getOpcode() == Instruction::SRem) {
            if (constRhs && constRhs->getSExtValue()==1) {
              binOp->replaceAllUsesWith(ConstantInt::get(rhs->getType(), 0));
              instToErase.push_back(binOp);
              ++strengthReductionTimes;
            }
          } else if (binOp->getOpcode() == Instruction::Add) {
            // workable
            if (constLhs && constLhs->getSExtValue()==0 && constRhs && constRhs->getSExtValue()==0) {
              binOp->replaceAllUsesWith(ConstantInt::get(lhs->getType(), 0));
              instToErase.push_back(binOp);
              ++strengthReductionTimes;

            } else if (constLhs && constLhs->getSExtValue()==0) {
              binOp->replaceAllUsesWith(rhs);
              instToErase.push_back(binOp);
              ++strengthReductionTimes;

            } else if (constRhs && constRhs->getSExtValue()==0) {
              binOp->replaceAllUsesWith(lhs);
              instToErase.push_back(binOp);
              ++strengthReductionTimes;

            }


          }
        }
      }

      // 统一删除标记的指令
      for (auto& i : instToErase)
        i->eraseFromParent();
    }
  }

  mOut << "StrengthReduction running...\nTo reduct " << strengthReductionTimes
       << " instructions\n";

  return PreservedAnalyses::all();
}