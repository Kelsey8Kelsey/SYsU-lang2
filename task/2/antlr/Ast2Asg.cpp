#include "Ast2Asg.hpp"
#include <unordered_map>

#define self (*this)

namespace asg {

// 符号表，保存当前作用域的所有声明
struct Ast2Asg::Symtbl : public std::unordered_map<std::string, Decl*>
{
  Ast2Asg& m;
  Symtbl* mPrev;

  Symtbl(Ast2Asg& m)
    : m(m)
    , mPrev(m.mSymtbl)
  {
    m.mSymtbl = this;
  }

  ~Symtbl() { m.mSymtbl = mPrev; }

  Decl* resolve(const std::string& name);
};

Decl*
Ast2Asg::Symtbl::resolve(const std::string& name)
{
  auto iter = find(name);
  if (iter != end())
    return iter->second;
  ASSERT(mPrev != nullptr); // 标识符未定义
  return mPrev->resolve(name);
}

TranslationUnit*
Ast2Asg::operator()(ast::TranslationUnitContext* ctx)
{
  auto ret = make<asg::TranslationUnit>();
  if (ctx == nullptr)
    return ret;

  Symtbl localDecls(self);

  for (auto&& i : ctx->externalDeclaration()) {
    if (auto p = i->declaration()) {
      auto decls = self(p);
      ret->decls.insert(ret->decls.end(),
                        std::make_move_iterator(decls.begin()),
                        std::make_move_iterator(decls.end()));
    }

    else if (auto p = i->functionDefinition()) {
      auto funcDecl = self(p);
      ret->decls.push_back(funcDecl);

      // 添加到声明表
      localDecls[funcDecl->name] = funcDecl;
    }

    else
      ABORT();
  }

  return ret;
}

//==============================================================================
// 类型
//==============================================================================

Ast2Asg::SpecQual
Ast2Asg::operator()(ast::DeclarationSpecifiersContext* ctx)
{
  SpecQual ret = { Type::Spec::kINVALID, Type::Qual() };

  for (auto&& i : ctx->declarationSpecifier()) {
    if (auto p = i->typeSpecifier()) {
      if (ret.first == Type::Spec::kINVALID || ret.second.const_ == false) {
        if (p->Int())
          ret.first = Type::Spec::kInt;
        else if (p->Void())
          ret.first = Type::Spec::kVoid;
        else if (p->Const())
          ret.second.const_ = true;
        else
          ABORT(); // 未知的类型说明符
      }

      else
        ABORT(); // 未知的类型说明符
    }

    else
      ABORT();
  }

  return ret;
}

std::pair<TypeExpr*, std::string>
Ast2Asg::operator()(ast::DeclaratorContext* ctx, TypeExpr* sub)
{
  return self(ctx->directDeclarator(), sub);
}

static int
eval_arrlen(Expr* expr)
{
  if (auto p = expr->dcst<IntegerLiteral>())
    return p->val;

  if (auto p = expr->dcst<DeclRefExpr>()) {
    if (p->decl == nullptr)
      ABORT();

    auto var = p->decl->dcst<VarDecl>();
    if (!var || !var->type->qual.const_)
      ABORT(); // 数组长度必须是编译期常量

    switch (var->type->spec) {
      case Type::Spec::kChar:
      case Type::Spec::kInt:
      case Type::Spec::kLong:
      case Type::Spec::kLongLong:
        return eval_arrlen(var->init);

      default:
        ABORT(); // 长度表达式必须是数值类型
    }
  }

  if (auto p = expr->dcst<UnaryExpr>()) {
    auto sub = eval_arrlen(p->sub);

    switch (p->op) {
      case UnaryExpr::kPos:
        return sub;

      case UnaryExpr::kNeg:
        return -sub;

      default:
        ABORT();
    }
  }

  if (auto p = expr->dcst<BinaryExpr>()) {
    auto lft = eval_arrlen(p->lft);
    auto rht = eval_arrlen(p->rht);

    switch (p->op) {
      case BinaryExpr::kAdd:
        return lft + rht;

      case BinaryExpr::kSub:
        return lft - rht;

      default:
        ABORT();
    }
  }

  if (auto p = expr->dcst<InitListExpr>()) {
    if (p->list.empty())
      return 0;
    return eval_arrlen(p->list[0]);
  }

  ABORT();
}

std::pair<TypeExpr*, std::string>
Ast2Asg::operator()(ast::DirectDeclaratorContext* ctx, TypeExpr* sub)
{
  if (auto p = ctx->Identifier())
    return { sub, p->getText() };

  if (ctx->LeftBracket()) {
    auto arrayType = make<ArrayType>();
    arrayType->sub = sub;

    if (auto p = ctx->assignmentExpression())
      arrayType->len = eval_arrlen(self(p));
    else
      arrayType->len = ArrayType::kUnLen;

    return self(ctx->directDeclarator(), arrayType);
  }
  
  if (ctx->LeftParen()) {
    // 这些操作在
    // FunctionDecl*
    // Ast2Asg::operator()(ast::FunctionDefinitionContext* ctx) 已经做过？
    // 上述操作只针对无参的函数声明
    // 对于有参数的函数声明我们将操作放到这里做而不是上面的函数里做

    auto functionType = make<FunctionType>();
    functionType->sub = sub;
    if (auto p = ctx->parameterList()) {
      for(auto&& i : p->parameterDeclaration()) {
        auto decl = self(i);
        functionType->params.push_back(decl->type);
      }
    }
    return self(ctx->directDeclarator(), functionType);


    // return self(ctx->directDeclarator(), sub);
  }

  ABORT();
}

//==============================================================================
// 表达式
//==============================================================================

Expr*
Ast2Asg::operator()(ast::ExpressionContext* ctx)
{
  auto list = ctx->assignmentExpression();
  Expr* ret = self(list[0]);

  for (unsigned i = 1; i < list.size(); ++i) {
    auto node = make<BinaryExpr>();
    node->op = node->kComma;
    node->lft = ret;
    node->rht = self(list[i]);
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::AssignmentExpressionContext* ctx)
{
  if (auto p = ctx->conditionalExpression())
    return self(p);

  if (ctx->unaryExpression()) {
    auto ret = make<BinaryExpr>();
    ret->op = ret->kAssign;
    ret->lft = self(ctx->unaryExpression());
    ret->rht = self(ctx->assignmentExpression());
    return ret;
  }

  ABORT();
}

Expr*
Ast2Asg::operator()(ast::ConditionalExpressionContext* ctx)
{
  if (auto p = ctx->logicOrExpression())
    return self(p);

  ABORT();
}

Expr*
Ast2Asg::operator()(ast::RelationalExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::AdditiveExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                   ->getSymbol()
                   ->getType();
    switch (token) {
      case ast::Greater:
        node->op = node->kGt;
        break;

      case ast::Less:
        node->op = node->kLt;
        break;

      case ast::Greaterequal:
        node->op = node->kGe;
        break;

      case ast::Lessequal:
        node->op = node->kLe;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = self(dynamic_cast<ast::AdditiveExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::EqualityExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::RelationalExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                   ->getSymbol()
                   ->getType();
    switch (token) {
      case ast::Equalequal:
        node->op = node->kEq;
        break;

      case ast::Exclaimequal:
        node->op = node->kNe;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = self(dynamic_cast<ast::RelationalExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::LogicAndExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::EqualityExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();
    node->op = node->kAnd;
    node->lft = ret;
    node->rht = self(dynamic_cast<ast::EqualityExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::LogicOrExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::LogicAndExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();
    node->op = node->kOr;
    node->lft = ret;
    node->rht = self(dynamic_cast<ast::LogicAndExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::AdditiveExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::MultiplicativeExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                   ->getSymbol()
                   ->getType();
    switch (token) {
      case ast::Plus:
        node->op = node->kAdd;
        break;

      case ast::Minus:
        node->op = node->kSub;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = self(dynamic_cast<ast::MultiplicativeExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}

Expr*
Ast2Asg::operator()(ast::MultiplicativeExpressionContext* ctx)
{
  auto children = ctx->children;
  Expr* ret = self(dynamic_cast<ast::UnaryExpressionContext*>(children[0]));

  for (unsigned i = 1; i < children.size(); ++i) {
    auto node = make<BinaryExpr>();

    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[i])
                   ->getSymbol()
                   ->getType();
    switch (token) {
      case ast::Star:
        node->op = node->kMul;
        break;

      case ast::Slash:
        node->op = node->kDiv;
        break;

      case ast::Percent:
        node->op = node->kMod;
        break;

      default:
        ABORT();
    }

    node->lft = ret;
    node->rht = self(dynamic_cast<ast::UnaryExpressionContext*>(children[++i]));
    ret = node;
  }

  return ret;
}


Expr*
Ast2Asg::operator()(ast::UnaryExpressionContext* ctx)
{
  if (auto p = ctx->postfixExpression())
    return self(p);

  if (ctx->unaryOperator()) {
    auto ret = make<UnaryExpr>();

    switch (
      dynamic_cast<antlr4::tree::TerminalNode*>(ctx->unaryOperator()->children[0])
        ->getSymbol()
        ->getType()) {
      case ast::Plus:
        ret->op = ret->kPos;
        break;

      case ast::Minus:
        ret->op = ret->kNeg;
        break;
      
      case ast::Exclaim:
        ret->op = ret->kNot;
        break;

      default:
        ABORT();
    }

    ret->sub = self(ctx->unaryExpression());

    return ret;
  }

  ABORT();
}

Expr*
Ast2Asg::operator()(ast::PostfixExpressionContext* ctx)
{
  auto children = ctx->children;
  auto sub = self(dynamic_cast<ast::PrimaryExpressionContext*>(children[0]));
  
  if (children.size()>1)  {
    auto token = dynamic_cast<antlr4::tree::TerminalNode*>(children[1])
                   ->getSymbol()
                   ->getType();
  
    if (token == ast::LeftBracket)  {
      for (unsigned i = 2; i < children.size(); i+=3) {
        auto node = make<BinaryExpr>();
        node->op = node->kIndex;
        node->lft = sub;
        node->rht = self(dynamic_cast<ast::AssignmentExpressionContext*>(children[i]));
        sub = node;
      }
    }

    else if (token == ast::LeftParen) {
      auto node = make<CallExpr>();
      node->head = sub;

      if (auto p = ctx->argumentExpressionList()) {
        for (auto&&j : p->assignmentExpression()) {
          node->args.push_back(self(j));
        }
      }
      sub = node;
    }
  }

  return sub;
}

Expr*
Ast2Asg::operator()(ast::PrimaryExpressionContext* ctx)
{

  if (auto p = ctx->Identifier()) {
    auto name = p->getText();
    auto ret = make<DeclRefExpr>();
    ret->decl = mSymtbl->resolve(name);
    return ret;
  }

  if (auto p = ctx->Constant()) {
    auto text = p->getText();

    auto ret = make<IntegerLiteral>();

    ASSERT(!text.empty());
    if (text[0] != '0')
      ret->val = std::stoll(text);

    else if (text.size() == 1)
      ret->val = 0;

    else if (text[1] == 'x' || text[1] == 'X')
      ret->val = std::stoll(text.substr(2), nullptr, 16);

    else
      ret->val = std::stoll(text.substr(1), nullptr, 8);

    return ret;
  }
  if (auto p = ctx->expression()) {
    auto ret = make<ParenExpr>();
    ret->sub = self(ctx->expression());
    return ret;
  }

  ABORT();
}

Expr*
Ast2Asg::operator()(ast::InitializerContext* ctx)
{
  if (auto p = ctx->assignmentExpression())
    return self(p);

  auto ret = make<InitListExpr>();

  if (auto p = ctx->initializerList()) {
    for (auto&& i : p->initializer()) {
      // 将初始化列表展平
      auto expr = self(i);
      if (auto p = expr->dcst<InitListExpr>()) {
        for (auto&& sub : p->list)
          ret->list.push_back(sub);
      } else {
        ret->list.push_back(expr);
      }
    }
  }

  return ret;
}

//==============================================================================
// 语句
//==============================================================================

Stmt*
Ast2Asg::operator()(ast::StatementContext* ctx)
{
  if (auto p = ctx->compoundStatement())
    return self(p);

  if (auto p = ctx->expressionStatement())
    return self(p);

  if (auto p = ctx->jumpStatement())
    return self(p);
  
  if (auto p = ctx->ifStatement())
    return self(p);

  if (auto p = ctx->whileStatement())
    return self(p);

  ABORT();
}

CompoundStmt*
Ast2Asg::operator()(ast::CompoundStatementContext* ctx)
{
  auto ret = make<CompoundStmt>();

  if (auto p = ctx->blockItemList()) {
    Symtbl localDecls(self);

    for (auto&& i : p->blockItem()) {
      if (auto q = i->declaration()) {
        auto sub = make<DeclStmt>();
        sub->decls = self(q);
        ret->subs.push_back(sub);
      }

      else if (auto q = i->statement())
        ret->subs.push_back(self(q));

      else
        ABORT();
    }
  }

  return ret;
}

Stmt*
Ast2Asg::operator()(ast::ExpressionStatementContext* ctx)
{
  if (auto p = ctx->expression()) {
    auto ret = make<ExprStmt>();
    ret->expr = self(p);
    return ret;
  }

  return make<NullStmt>();
}

Stmt*
Ast2Asg::operator()(ast::JumpStatementContext* ctx)
{
  if (ctx->Return()) {
    auto ret = make<ReturnStmt>();
    ret->func = mCurrentFunc;
    if (auto p = ctx->expression())
      ret->expr = self(p);
    return ret;
  }

  if (ctx->Break()) {
    auto ret = make<BreakStmt>();
    // ret->loop = ;
    return ret;
  }

  if (ctx->Continue()) {
    auto ret = make<ContinueStmt>();
    // ret->loop = ;
    return ret;
  }

  ABORT();
}

Stmt*
Ast2Asg::operator()(ast::IfStatementContext* ctx)
{
  if (ctx->If())  {
    auto ret = make<IfStmt>();
    auto list = ctx->statement();
    ret->cond = self(ctx->expression());
    ret->then = self(list[0]);

    if (ctx->Else()) {
      ret->else_ = self(list[1]);
    }

    return ret;
  }

  ABORT();
}

Stmt*
Ast2Asg::operator()(ast::WhileStatementContext* ctx)
{
  if (ctx->While()) {
    auto ret = make<WhileStmt>();
    ret->cond = self(ctx->expression());
    ret->body = self(ctx->statement());

    return ret;
  }
  
  ABORT();
}

//==============================================================================
// 声明
//==============================================================================

std::vector<Decl*>
Ast2Asg::operator()(ast::DeclarationContext* ctx)
{
  std::vector<Decl*> ret;

  auto specs = self(ctx->declarationSpecifiers());

  if (auto p = ctx->initDeclaratorList()) {
    for (auto&& j : p->initDeclarator())
      ret.push_back(self(j, specs));
  }

  // 如果 initDeclaratorList 为空则这行声明语句无意义
  return ret;
}

// FunctionDecl*
// Ast2Asg::operator()(ast::FunctionDefinitionContext* ctx)
// {
//   auto ret = make<FunctionDecl>();
//   mCurrentFunc = ret;

//   auto type = make<Type>();
//   ret->type = type;

//   auto sq = self(ctx->declarationSpecifiers());
//   type->spec = sq.first, type->qual = sq.second;

//   auto [texp, name] = self(ctx->directDeclarator(), nullptr);
//   auto funcType = make<FunctionType>();
//   funcType->sub = texp;
//   type->texp = funcType;
//   ret->name = std::move(name);

//   Symtbl localDecls(self);

//   // 函数定义在签名之后就加入符号表，以允许递归调用
//   (*mSymtbl)[ret->name] = ret;

//   if (auto p = ctx->directDeclarator()->parameterList()) {
//     for(auto&& i : p->parameterDeclaration()) {
//       auto decl = self(i);
      
//       funcType->params.push_back(decl->type);
//       ret->params.push_back(decl);
      
//       (*mSymtbl)[decl->name] = decl;
//     }
//   }

//   // 注意将该语句放在上述参数声明之后！！ （卡了n天）
//   ret->body = self(ctx->compoundStatement());

//   return ret;
// }

FunctionDecl*
Ast2Asg::operator()(ast::FunctionDefinitionContext* ctx)
{
  auto ret = make<FunctionDecl>();
  mCurrentFunc = ret;

  auto type = make<Type>();
  ret->type = type;

  auto sq = self(ctx->declarationSpecifiers());
  type->spec = sq.first, type->qual = sq.second;

  auto [texp, name] = self(ctx->directDeclarator(), nullptr);
  // 原先下面的工作交给
  //    std::pair<TypeExpr*, std::string>
  //    Ast2Asg::operator()(ast::DirectDeclaratorContext* ctx, TypeExpr* sub)
  // 做
  // auto funcType = make<FunctionType>();
  // funcType->sub = texp;
  // type->texp = funcType;
  type->texp = texp;
  ret->name = std::move(name);

  Symtbl localDecls(self);

  // 函数定义在签名之后就加入符号表，以允许递归调用
  (*mSymtbl)[ret->name] = ret;

  // 填充FunctionDecl的参数列表，并把参数声明放入符号表中
  if (auto p = ctx->directDeclarator()->parameterList()) {
    for(auto&& i : p->parameterDeclaration()) {
      auto decl = self(i);
      
      ret->params.push_back(decl);
      
      (*mSymtbl)[decl->name] = decl;
    }
  }

  // 注意将该语句放在上述参数声明之后！！
  ret->body = self(ctx->compoundStatement());

  return ret;
}

Decl*
Ast2Asg::operator()(ast::InitDeclaratorContext* ctx, SpecQual sq)
{
  auto [texp, name] = self(ctx->declarator(), nullptr);
  Decl* ret;

  // 函数声明（不包含复合语句） typeSpecifier identifier (parameterList);
  if (auto funcType = texp->dcst<FunctionType>()) {
    auto fdecl = make<FunctionDecl>();
    auto type = make<Type>();
    fdecl->type = type;

    type->spec = sq.first;
    type->qual = sq.second;
    type->texp = funcType;

    fdecl->name = std::move(name);

    // for (auto p : funcType->params) {
    //   auto paramDecl = make<VarDecl>();
    //   paramDecl->type = p;
    //   // 漏了名字！
    //   // 怎么获取参数的名字？ FunctionType.params中是Type *, 不含名字
    //   // 考虑下面的实现：重新从参数列表parameterList中构建VarDecl
    //   // paramDecl->name = ???;

    //   fdecl->params.push_back(paramDecl);
    // }

    if (auto x = ctx->declarator()->directDeclarator()->parameterList()) {
        for(auto&& i : x->parameterDeclaration()) {
          auto decl = self(i);
          auto paramDecl = make<VarDecl>();
          paramDecl = decl->dcst<VarDecl>();
          // 等价于
          // paramDecl->type = decl->type;
          // paramDecl->name = decl->name;
          fdecl->params.push_back(paramDecl);
        }
    }

    if (ctx->initializer())
      ABORT();
    fdecl->body = nullptr;

    ret = fdecl;
  }

  else {
    auto vdecl = make<VarDecl>();
    auto type = make<Type>();
    vdecl->type = type;

    type->spec = sq.first;
    type->qual = sq.second;
    type->texp = texp;
    vdecl->name = std::move(name);

    if (auto p = ctx->initializer())
      vdecl->init = self(p);
    else
      vdecl->init = nullptr;

    ret = vdecl;
  }

  // 这个实现允许符号重复定义，新定义会取代旧定义
  (*mSymtbl)[ret->name] = ret;
  return ret;
}

Decl*   // 返回参数的声明VarDecl
Ast2Asg::operator()(ast::ParameterDeclarationContext* ctx)
{
  Decl* ret;
  auto specs = self(ctx->declarationSpecifiers());
  auto [texp, name] = self(ctx->declarator(), nullptr);

  auto vdecl = make<VarDecl>();

  auto type = make<Type>();
  vdecl->type = type;

  type->spec = specs.first;
  type->qual = specs.second;
  type->texp = texp;

  vdecl->name = std::move(name);
  vdecl->init = nullptr;

  ret = vdecl;

  // (*mSymtbl)[ret->name] = ret;

  return ret;
}

} // namespace asg
