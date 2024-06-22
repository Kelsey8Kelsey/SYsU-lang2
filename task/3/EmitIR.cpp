#include "EmitIR.hpp"
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <llvm/IR/ValueSymbolTable.h>

#define self (*this)

using namespace asg;

EmitIR::EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid)
  : mMgr(mgr)
  , mMod(mid, ctx)
  , mCtx(ctx)
  , mIntTy(llvm::Type::getInt32Ty(ctx))
  , mCurIrb(std::make_unique<llvm::IRBuilder<>>(ctx))
  , mCtorTy(llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), false))
{
}

llvm::Module&
EmitIR::operator()(asg::TranslationUnit* tu)
{
  for (auto&& i : tu->decls)
    self(i);
  return mMod;
}

//==============================================================================
// 类型
//==============================================================================

llvm::Type*
EmitIR::operator()(const Type* type)
{
  if (type->texp == nullptr) {  // texp为空
    switch (type->spec) {
      case Type::Spec::kInt:
        return llvm::Type::getInt32Ty(mCtx);
      // TODO: 在此添加对更多基础类型的处理
      case Type::Spec::kVoid:
        return llvm::Type::getVoidTy(mCtx);

      default:
        ABORT();
    }
  }

  Type subt;
  subt.spec = type->spec;
  subt.qual = type->qual;
  subt.texp = type->texp->sub;

  // TODO: 在此添加对指针类型、数组类型和函数类型的处理

  if (auto p = type->texp->dcst<FunctionType>()) {
    std::vector<llvm::Type*> pty;
    // TODO: 在此添加对函数参数类型的处理 ***注意：数组类型的参数***
    for(auto &&a : p->params)
    {
      pty.push_back(self(a));
    }

    return llvm::FunctionType::get(self(&subt), std::move(pty), false);
  }

  if (auto p = type->texp->dcst<ArrayType>())
  {
    return llvm::ArrayType::get(self(&subt), p->len);
  }

  if (auto p = type->texp->dcst<PointerType>())
  {
    return llvm::PointerType::get(mCtx, 0);
  }

  ABORT();
}

//==============================================================================
// 表达式
//==============================================================================

llvm::Value*
EmitIR::operator()(Expr* obj)
{
  // TODO: 在此添加对更多表达式处理的跳转
  if (auto p = obj->dcst<IntegerLiteral>())
    return self(p);
  
  if (auto p = obj->dcst<DeclRefExpr>())
    return self(p);

  if (auto p = obj->dcst<ImplicitCastExpr>())
    return self(p);

  if (auto p = obj->dcst<BinaryExpr>())
    return self(p);
  
  if (auto p = obj->dcst<UnaryExpr>())
    return self(p);

  if (auto p = obj->dcst<ParenExpr>())
    return self(p);

  if (auto p = obj->dcst<CallExpr>())
    return self(p);

  ABORT();
}

llvm::Value*
EmitIR::operator()(ImplicitCastExpr* obj)
{
  auto sub = self(obj->sub);

  auto& irb = *mCurIrb;
  switch (obj->kind) {
    case ImplicitCastExpr::kLValueToRValue: {
      // if (obj->sub->dcst<DeclRefExpr>()->decl->type->qual.const_) // 常量  优化：直接替换常量，不再load
      // {
      //   return sub;

      // } else 
      // {
      //   auto ty = self(obj->sub->type);
      //   auto loadVal = irb.CreateLoad(ty, sub);
      //   return loadVal;
      // }

      auto ty = self(obj->sub->type);
      auto loadVal = irb.CreateLoad(ty, sub);
      return loadVal;
    }

    case ImplicitCastExpr::kFunctionToPointerDecay: 
    {
      return self(obj->sub);
    }
    case ImplicitCastExpr::kArrayToPointerDecay:
    {
      return self(obj->sub);
    }

    default:
      ABORT();
  }
}

llvm::Value*
EmitIR::operator()(DeclRefExpr* obj)
{
  // 在LLVM IR层面，左值体现为返回指向值的指针
  // 在ImplicitCastExpr::kLValueToRValue中发射load指令从而变成右值
  return reinterpret_cast<llvm::Value*>(obj->decl->any);
}

llvm::Value*
EmitIR::operator()(BinaryExpr* obj)
{
  if (obj->op != BinaryExpr::kAnd && obj->op != BinaryExpr::kOr)
  {
    llvm::Value *lftVal, *rhtVal;

    lftVal = self(obj->lft);
    auto& irb = *mCurIrb;
    rhtVal = self(obj->rht);

    switch (obj->op) {
      case BinaryExpr::kAdd:
        return irb.CreateAdd(lftVal, rhtVal, "", false, true);
      case BinaryExpr::kSub:
        return irb.CreateSub(lftVal, rhtVal, "", false, true);
      case BinaryExpr::kMul:
        return irb.CreateMul(lftVal, rhtVal, "", false, true);
      case BinaryExpr::kDiv:
        return irb.CreateSDiv(lftVal, rhtVal);
      case BinaryExpr::kAssign:
        return irb.CreateStore(rhtVal, lftVal);
      case BinaryExpr::kMod:
        return irb.CreateSRem(lftVal, rhtVal);
      case BinaryExpr::kGt:
        return irb.CreateICmpSGT(lftVal, rhtVal);
      case BinaryExpr::kGe:
        return irb.CreateICmpSGE(lftVal, rhtVal);
      case BinaryExpr::kLt:
        return irb.CreateICmpSLT(lftVal, rhtVal);
      case BinaryExpr::kLe:
        return irb.CreateICmpSLE(lftVal, rhtVal);
      case BinaryExpr::kEq:
        return irb.CreateICmpEQ(lftVal, rhtVal);
      case BinaryExpr::kNe:
        return irb.CreateICmpNE(lftVal, rhtVal);
      case BinaryExpr::kIndex:    // == ArrayScriptExpr
      {
        // *** 区分type
        llvm::Type* ty = self(obj->lft->dcst<ImplicitCastExpr>()->sub->type);
        if (ty->isArrayTy())
        {
          std::vector<llvm::Value *> idxList;
          idxList.push_back(irb.getInt64(0));
          llvm::Value* rhtVal_64 = irb.CreateSExt(rhtVal, llvm::Type::getInt64Ty(mCtx)); // 符号扩展
          idxList.push_back(rhtVal_64);
          return irb.CreateInBoundsGEP(ty, lftVal, std::move(idxList));

        } else if (ty->isPointerTy())   // 在函数中访问指向数组的指针
        {
          ty = llvm::Type::getInt32Ty(mCtx);  // 写死  TODO:改进
          std::vector<llvm::Value *> idxList;
          llvm::Value* rhtVal_64 = irb.CreateSExt(rhtVal, llvm::Type::getInt64Ty(mCtx)); // 符号扩展
          idxList.push_back(rhtVal_64);
          return irb.CreateInBoundsGEP(ty, lftVal, std::move(idxList));
        }
      }

    default:
      ABORT();
    }

  } else  // 短路运算
  {
    // if (obj->op == BinaryExpr::kAnd)   // 不使用短路计算
    // {
    //   llvm::Value *lftVal, *rhtVal;
    //   lftVal = self(obj->lft);
    //   auto& irb = *mCurIrb;
    //   rhtVal = self(obj->rht);
    //   llvm::Value * boolLftVal = lftVal;
    //   llvm::Value * boolRhtVal = rhtVal;

    //   if (!lftVal->getType()->isIntegerTy(1))
    //   {
    //     boolLftVal = irb.CreateICmpNE(lftVal, llvm::ConstantInt::get(lftVal->getType(), 0));
    //   }
    //   if (!rhtVal->getType()->isIntegerTy(1))
    //   {
    //     boolRhtVal = irb.CreateICmpNE(rhtVal, llvm::ConstantInt::get(rhtVal->getType(), 0));
    //   }

    //   // 使用位与运算实现逻辑与
    //   return irb.CreateAnd(boolLftVal, boolRhtVal);

    // } else if (obj->op == BinaryExpr::kOr)   // 不使用短路运算
    // {
    //   llvm::Value *lftVal, *rhtVal;
    //   lftVal = self(obj->lft);
    //   auto& irb = *mCurIrb;
    //   rhtVal = self(obj->rht);
    //   llvm::Value * boolLftVal = lftVal;
    //   llvm::Value * boolRhtVal = rhtVal;

    //   if (!lftVal->getType()->isIntegerTy(1))
    //   {
    //     boolLftVal = irb.CreateICmpNE(lftVal, llvm::ConstantInt::get(lftVal->getType(), 0));
    //   }
    //   if (!rhtVal->getType()->isIntegerTy(1))
    //   {
    //     boolRhtVal = irb.CreateICmpNE(rhtVal, llvm::ConstantInt::get(rhtVal->getType(), 0));
    //   }

    //   // 使用位或运算实现逻辑或
    //   return irb.CreateOr(boolLftVal, boolRhtVal);
    // }

    // ****************************************************
    if (obj->op == BinaryExpr::kAnd)    // 使用短路计算
    {
      llvm::Value *lftVal, *rhtVal;
      lftVal = self(obj->lft);
      auto& irb = *mCurIrb;
      

      if (!lftVal->getType()->isIntegerTy(1))
      {
        lftVal = irb.CreateICmpNE(lftVal, llvm::ConstantInt::get(lftVal->getType(), 0));
      }

      llvm::BasicBlock *lftTrueBlock = llvm::BasicBlock::Create(mCtx, "land.rht", mCurFunc);
      llvm::BasicBlock *landEndBlock = llvm::BasicBlock::Create(mCtx, "land.end", mCurFunc);

      llvm::BasicBlock * currentBlock = irb.GetInsertBlock();

      irb.CreateCondBr(lftVal, lftTrueBlock, landEndBlock);

      irb.SetInsertPoint(lftTrueBlock);

      rhtVal = self(obj->rht);

      llvm::BasicBlock * currentRhtBlock = irb.GetInsertBlock();

      if (!rhtVal->getType()->isIntegerTy(1))
      {
        rhtVal = irb.CreateICmpNE(rhtVal, llvm::ConstantInt::get(rhtVal->getType(), 0));
      }

      irb.CreateBr(landEndBlock);

      irb.SetInsertPoint(landEndBlock);

      // 使用phi指令
      llvm::PHINode *phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2);
      phi->addIncoming(irb.getInt1(false), currentBlock);
      phi->addIncoming(rhtVal, currentRhtBlock);

      return phi;

    }
  
    else if (obj->op == BinaryExpr::kOr)  // 使用短路运算
    {
      llvm::Value *lftVal, *rhtVal;
      lftVal = self(obj->lft);
      auto& irb = *mCurIrb;

      if (!lftVal->getType()->isIntegerTy(1))
      {
        lftVal = irb.CreateICmpNE(lftVal, llvm::ConstantInt::get(lftVal->getType(), 0));
      }

      llvm::BasicBlock *lftFalseBlock = llvm::BasicBlock::Create(mCtx, "land.rht", mCurFunc);
      llvm::BasicBlock *landEndBlock = llvm::BasicBlock::Create(mCtx, "land.end", mCurFunc);

      llvm::BasicBlock * currentBlock = irb.GetInsertBlock();

      irb.CreateCondBr(lftVal, landEndBlock, lftFalseBlock);

      irb.SetInsertPoint(lftFalseBlock);

      rhtVal = self(obj->rht);

      llvm::BasicBlock * currentRhtBlock = irb.GetInsertBlock();

      if (!rhtVal->getType()->isIntegerTy(1))
      {
        rhtVal = irb.CreateICmpNE(rhtVal, llvm::ConstantInt::get(rhtVal->getType(), 0));
      }

      irb.CreateBr(landEndBlock);

      irb.SetInsertPoint(landEndBlock);

      // 使用phi指令
      llvm::PHINode *phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2);
      phi->addIncoming(irb.getInt1(true), currentBlock);
      phi->addIncoming(rhtVal, currentRhtBlock);

      return phi;
    }
    // *******************************************************

  }

}

llvm::Value*
EmitIR::operator()(UnaryExpr* obj)
{
  auto& irb = *mCurIrb;

  switch(obj->op) {
    case UnaryExpr::kNeg:
      // return irb.CreateNeg(self(obj->sub));
    {
      llvm::Value* val = self(obj->sub);
      llvm::Type* exprType = val->getType();
      if (!exprType->isIntegerTy(1))
      {
        return irb.CreateNeg(self(obj->sub));
      }else
      {
        auto extVal = irb.CreateZExt(val, llvm::Type::getInt32Ty(mCtx));
        return irb.CreateNeg(extVal, "", false, true);
      }

    }
    case UnaryExpr::kNot:
      // return irb.CreateNot(self(obj->sub));
    {
      llvm::Value* val = self(obj->sub);
      llvm::Type* exprType = val->getType();
      if (exprType->isIntegerTy(1))
      {
        return irb.CreateNot(val);
      }else
      {
        llvm::Value* boolVal = irb.CreateICmpNE(val, llvm::ConstantInt::get(exprType, 0));
        return irb.CreateNot(boolVal);
      }

    }
    case UnaryExpr::kPos:
      return self(obj->sub);

    default:
      ABORT();
  }

}

llvm::Value*
EmitIR::operator()(ParenExpr* obj)
{
  return self(obj->sub);
}

llvm::Value*
EmitIR::operator()(CallExpr* obj)
{
  auto& irb = *mCurIrb;
  std::vector<llvm::Value *> args;

  for (auto&&p : obj->args)
  {
    args.push_back(self(p));
  }

  return irb.CreateCall(mMod.getFunction(self(obj->head)->getName()), args);
}

llvm::Constant*
EmitIR::operator()(IntegerLiteral* obj)
{
  return llvm::ConstantInt::get(self(obj->type), obj->val);
}

// TODO: 在此添加对更多表达式类型的处理

//==============================================================================
// 语句
//==============================================================================

void
EmitIR::operator()(Stmt* obj)
{
  // TODO: 在此添加对更多Stmt类型的处理的跳转
  if (auto p = obj->dcst<CompoundStmt>())
    return self(p);

  if (auto p = obj->dcst<ReturnStmt>())
    return self(p);
  
  if (auto p = obj->dcst<DeclStmt>())
    return self(p);

  if (auto p = obj->dcst<ExprStmt>())
    return self(p);

  if (auto p = obj->dcst<IfStmt>())
    return self(p);

  if (auto p = obj->dcst<WhileStmt>())
    return self(p);

  if (auto p = obj->dcst<BreakStmt>())
    return self(p);

  if (auto p = obj->dcst<ContinueStmt>())
    return self(p);
  
  if (auto p = obj->dcst<NullStmt>())
    return;

  ABORT();
}

// TODO: 在此添加对更多Stmt类型的处理

void
EmitIR::operator()(CompoundStmt* obj)
{
  // TODO: 可以在此添加对符号重名的处理
  for (auto&& stmt : obj->subs)
    self(stmt);
}

void
EmitIR::operator()(ReturnStmt* obj)
{
  auto& irb = *mCurIrb;

  llvm::Value* retVal;
  if (!obj->expr)
    retVal = nullptr;
  else
    retVal = self(obj->expr);

  // auto temp = irb.GetInsertBlock();

  // llvm::BasicBlock *retBlock = llvm::BasicBlock::Create(mCtx, "return", mCurFunc);

  // irb.CreateBr(retBlock);

  mCurIrb->CreateRet(retVal);

  // auto exitBb = llvm::BasicBlock::Create(mCtx, "return_exit", mCurFunc);
  // mCurIrb = std::make_unique<llvm::IRBuilder<>>(exitBb);
}

void
EmitIR::operator()(DeclStmt* obj)
{
  for (auto&& decl : obj->decls)
    self(decl);
}

void
EmitIR::operator()(ExprStmt* obj)
{
  self(obj->expr);
}

void
EmitIR::operator()(IfStmt* obj)
{
  auto& irb = *mCurIrb;
  auto val = self(obj->cond);


  llvm::BasicBlock *ifThenBlock = llvm::BasicBlock::Create(mCtx, "if.then", mCurFunc);
  
  if (obj->else_)
  {
    auto temp = irb.GetInsertBlock();
    bool deterThen = false;
    bool deterElse = false;
    llvm::BasicBlock * insertBlockThen;
    llvm::BasicBlock * insertBlockElse;

    irb.SetInsertPoint(ifThenBlock);
    self(obj->then);

    if (!irb.GetInsertBlock()->getTerminator())
    {
      deterThen = true;
      insertBlockThen = irb.GetInsertBlock();
    }

    llvm::BasicBlock *ifElseBlock = llvm::BasicBlock::Create(mCtx, "if.else", mCurFunc);

    irb.SetInsertPoint(ifElseBlock);
    self(obj->else_);

    if (!irb.GetInsertBlock()->getTerminator())
    {
      deterElse = true;
      insertBlockElse = irb.GetInsertBlock();
    }

    llvm::BasicBlock *ifEndBlock = llvm::BasicBlock::Create(mCtx, "if.end", mCurFunc);

    if (deterThen)
    {
      irb.SetInsertPoint(insertBlockThen);
      irb.CreateBr(ifEndBlock);
    }

    if (deterElse)
    {
      irb.SetInsertPoint(insertBlockElse);
      irb.CreateBr(ifEndBlock);
    }

    irb.SetInsertPoint(temp);
    if (!val->getType()->isIntegerTy(1))
    {
      val = irb.CreateICmpNE(val, llvm::ConstantInt::get(val->getType(), 0));
    }
    irb.CreateCondBr(val, ifThenBlock, ifElseBlock);

    irb.SetInsertPoint(ifEndBlock);
  }
  else
  {
    auto temp = irb.GetInsertBlock();

    irb.SetInsertPoint(ifThenBlock);
    self(obj->then);

    llvm::BasicBlock *ifEndBlock = llvm::BasicBlock::Create(mCtx, "if.end", mCurFunc);
    
    if (!irb.GetInsertBlock()->getTerminator())
    {
      irb.CreateBr(ifEndBlock);
    }

    irb.SetInsertPoint(temp);
    irb.CreateCondBr(val, ifThenBlock, ifEndBlock);

    irb.SetInsertPoint(ifEndBlock);
  }
  
  
}

// void
// EmitIR::operator()(WhileStmt* obj)
// {
//   auto& irb = *mCurIrb;

//   llvm::BasicBlock *whileCondBlock = llvm::BasicBlock::Create(mCtx, "while.cond", mCurFunc);

//   irb.CreateBr(whileCondBlock);

//   irb.SetInsertPoint(whileCondBlock);
//   llvm::Value* val = self(obj->cond);

//   llvm::BasicBlock *whileBodyBlock = llvm::BasicBlock::Create(mCtx, "while.body", mCurFunc);

//   irb.SetInsertPoint(whileBodyBlock);
//   self(obj->body);
//   irb.CreateBr(whileCondBlock);

//   llvm::BasicBlock *whileEndBlock = llvm::BasicBlock::Create(mCtx, "while.end", mCurFunc);

//   irb.SetInsertPoint(whileCondBlock);
//   irb.CreateCondBr(val, whileBodyBlock, whileEndBlock);

//   irb.SetInsertPoint(whileEndBlock);
// }

void
EmitIR::operator()(WhileStmt* obj)
{
  llvm::BasicBlock *whileCondBlock = llvm::BasicBlock::Create(mCtx, "while.cond", mCurFunc);
  llvm::BasicBlock *whileBodyBlock = llvm::BasicBlock::Create(mCtx, "while.body", mCurFunc);
  llvm::BasicBlock *whileEndBlock = llvm::BasicBlock::Create(mCtx, "while.end", mCurFunc);

  obj->any = new whileBlock((void *)whileEndBlock, (void *)whileBodyBlock);

  mCurIrb->CreateBr(whileCondBlock);

  mCurIrb->SetInsertPoint(whileCondBlock);
  llvm::Value* val = self(obj->cond);

  // mCurIrb = std::make_unique<llvm::IRBuilder<>>(whileBodyBlock);

  mCurIrb->SetInsertPoint(whileBodyBlock);
  self(obj->body);
  mCurIrb->CreateBr(whileCondBlock);

  // mCurIrb = std::make_unique<llvm::IRBuilder<>>(whileEndBlock);

  mCurIrb->SetInsertPoint(whileCondBlock);
  mCurIrb->CreateCondBr(val, whileBodyBlock, whileEndBlock);

  mCurIrb->SetInsertPoint(whileEndBlock);

  delete((whileBlock* )obj->any);
  obj->any = nullptr;
}

void
EmitIR::operator()(BreakStmt* obj)
{
  auto temp = (struct whileBlock * )obj->loop->any;
  mCurIrb->CreateBr((llvm::BasicBlock*)temp->whileEndBlock);
}

void
EmitIR::operator()(ContinueStmt* obj)
{
  auto temp = (struct whileBlock * )obj->loop->any;
  mCurIrb->CreateBr((llvm::BasicBlock*)temp->whileCondBlock);
}

//==============================================================================
// 声明
//==============================================================================

void
EmitIR::operator()(Decl* obj)
{

  if (auto p = obj->dcst<VarDecl>())
    return self(p);

  if (auto p = obj->dcst<FunctionDecl>())
    return self(p);

  ABORT();
}

void
EmitIR::trans_init(llvm::Value* val, Expr* obj)
{
  auto& irb = *mCurIrb;

  // 仅处理整数字面量的初始化
  if (auto p = obj->dcst<IntegerLiteral>()) {
    auto initVal = llvm::ConstantInt::get(self(p->type), p->val);
    irb.CreateStore(initVal, val);
    return;

  } else if (auto p = obj->dcst<UnaryExpr>())
  {
    auto initVal = llvm::ConstantInt::get(self(p->type), -p->sub->dcst<IntegerLiteral>()->val);
    irb.CreateStore(initVal, val);
    return;
  }

  // 如果表达式不是整数字面量，则中断编译
  ABORT();
}

void
EmitIR::operator()(VarDecl* obj)
{
  if (!mCurIrb->GetInsertBlock())  // 全局变量或常量
  {
    if (obj->type->qual.const_)   // 全局常量
    {
      if (obj->type->texp == nullptr)
      {
        if (obj->init != nullptr) // 常量在声明时必须初始化
        {
          llvm::Constant* constInt = llvm::ConstantInt::get(self(obj->type), obj->init->dcst<IntegerLiteral>()->val);
          // llvm::Constant* constInt = self(obj->init->dcst<IntegerLiteral>());
          // obj->any = constInt;  // 优化：不再load
          llvm::GlobalVariable *globalConst = new llvm::GlobalVariable(
            mMod, constInt->getType(), true, llvm::GlobalVariable::ExternalLinkage, constInt, obj->name);
          obj->any = globalConst; // 可读性
          globalConst->setAlignment(llvm::Align(4));
        } else
        {
          ABORT();
        }
        
      } else if (auto p = obj->type->texp->dcst<ArrayType>())
      {
        if (obj->init != nullptr) // 常量在声明时必须初始化
        {
          std::vector<llvm::Constant*> initList;

          for (auto&& it:obj->init->dcst<InitListExpr>()->list)
          {
            // only 一维
            initList.push_back(llvm::ConstantInt::get(self(it->type), it->dcst<IntegerLiteral>()->val));
          }

          llvm::Constant * constArray = llvm::ConstantArray::get((llvm::ArrayType *)self(obj->type), std::move(initList));
          // obj->any = constArray; // 优化：不再load
          llvm::GlobalVariable *globalArray = new llvm::GlobalVariable(
            mMod, constArray->getType(), true, llvm::GlobalVariable::ExternalLinkage, constArray, obj->name);
          obj->any = globalArray; // 可读性
          globalArray->setAlignment(llvm::Align(16));

        } else
        {
          ABORT();
        }
        
      }

    } else  // 全局变量
    {

    auto type = self(obj->type);
    auto gvar = new llvm::GlobalVariable(
      mMod, type, false, llvm::GlobalVariable::ExternalLinkage, nullptr, obj->name);

    // 留给遍历器存放任意数据
    obj->any = gvar;

    // 默认初始化为 0
    if (obj->type->texp == nullptr)
    {
      gvar->setInitializer(llvm::ConstantInt::get(type, 0));
      gvar->setAlignment(llvm::Align(4));

    } else if (auto p = obj->type->texp->dcst<ArrayType>()) 
    {
      gvar->setInitializer(llvm::ConstantAggregateZero::get(type));
      gvar->setAlignment(llvm::Align(16));
    }

    if (obj->init == nullptr)
      return;

    if (obj->type->texp == nullptr)
    {
      // 创建构造函数用于初始化
      mCurFunc = llvm::Function::Create(
        mCtorTy, llvm::GlobalVariable::PrivateLinkage, "ctor_" + obj->name, mMod);
      llvm::appendToGlobalCtors(mMod, mCurFunc, 65535);

      auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", mCurFunc);
      mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
      trans_init(gvar, obj->init);
      mCurIrb->CreateRet(nullptr);

      // IRBuilder退出上述基本块
      mCurIrb = std::make_unique<llvm::IRBuilder<>>(mCtx);
    } else if (obj->type->texp->dcst<ArrayType>())  // TODO:全局数组的初始化
    {

    }
    
    }
  }
  else  // 局部变量
  {
    auto lvar = mCurIrb->CreateAlloca(self(obj->type), nullptr, obj->name);

    obj->any = lvar;

    if (obj->type->texp == nullptr)
    {
      lvar->setAlignment(llvm::Align(4));
    } else if (obj->type->texp->dcst<ArrayType>())
    {
      lvar->setAlignment(llvm::Align(16));
    }

    if (obj->init == nullptr)
      return;
    
    if (obj->type->texp == nullptr)
    {
      auto val = self(obj->init);
      mCurIrb->CreateStore(val, lvar);

    } else if (obj->type->texp->dcst<ArrayType>())  // TODO 局部数组的初始化
    {
      for (auto&&initExpr : obj->init->dcst<InitListExpr>()->list)
      {
        auto initVal = self(initExpr);
        // mCurIrb->CreateStore(val, ?);
      }

      // for (unsigned i = 0; i < 3; ++i) {
      //   for (unsigned j = 0; j < 3; ++j) {
      //       // 索引值
      //       Value *indexI = builder.getInt32(i);
      //       Value *indexJ = builder.getInt32(j);

      //       // 获取二维数组中每个元素的指针
      //       Value *elementPtr = builder.CreateGEP(arrayAlloc, {builder.getInt32(0), indexI, indexJ});
            
      //       // 存储初始值0到每个数组元素中
      //       builder.CreateStore(builder.getInt32(0), elementPtr);

    }
    
  }
  
}

void
EmitIR::operator()(FunctionDecl* obj)
{
  // 创建函数
  auto fty = llvm::dyn_cast<llvm::FunctionType>(self(obj->type));
  auto func = llvm::Function::Create(
    fty, llvm::GlobalVariable::ExternalLinkage, obj->name, mMod);

  obj->any = func;

  if (obj->body == nullptr)
    return;
  auto entryBb = llvm::BasicBlock::Create(mCtx, "entry", func);
  mCurIrb = std::make_unique<llvm::IRBuilder<>>(entryBb);
  auto& entryIrb = *mCurIrb;

  if (obj->name == "main")
  {
    // mRet = mCurIrb->CreateAlloca(llvm::Type::getInt32Ty(mCtx), nullptr, "retval");
    // mCurIrb->CreateStore(mCurIrb->getInt32(0), (llvm::Value*)mRet);
  }

  // TODO: 添加对函数参数的处理
  auto argIter = func->arg_begin();
  for (auto&&p : obj->params)
  {
    argIter++->setName(p->name); 
    self(p);
  }

  for (auto&&p : obj->params)
  {
    mCurIrb->CreateStore(func->getValueSymbolTable()->lookup(p->name), (llvm::Value *)p->any);
  }

  // 翻译函数体
  mCurFunc = func;
  self(obj->body);
  auto& exitIrb = *mCurIrb;

  // 避免无终结指令的基本块出现
  if (!exitIrb.GetInsertBlock()->getTerminator())
  {
    exitIrb.CreateRet(exitIrb.getInt32(0));
  }

  if (fty->getReturnType()->isVoidTy())
    exitIrb.CreateRetVoid();
  else
    return; 
    // exitIrb.CreateUnreachable();
}
