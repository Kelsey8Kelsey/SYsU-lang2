parser grammar SYsUParser;

options {
  tokenVocab=SYsULexer;
}

primaryExpression
    :   Identifier
    |   Constant
    |   LeftParen expression RightParen
    ;

argumentExpressionList
    :   assignmentExpression (Comma assignmentExpression)*
    ;

postfixExpression
    // :   primaryExpression
    // // |   postfixExpression LeftBracket expression RightBracket
    :   primaryExpression (LeftBracket assignmentExpression RightBracket)*
    |   primaryExpression (LeftParen argumentExpressionList? RightParen)
    // |   primaryExpression (LeftParen argumentExpressionList? RightParen)* 上述为简单情况，实验测例仅包含上述情况
    ;

unaryExpression
    :   postfixExpression
    |   unaryOperator unaryExpression
    ;

unaryOperator
    :   Plus | Minus | Exclaim
    ;

// cast_expression  // 显式类型转换（实验测例不涉及）
// 	:   unary_expression
// 	|   '(' type_name ')' cast_expression
// 	;

// multiplicative_expression
// 	: cast_expression ((Star|Slash|Percent) cast_expression)*
//     ;

multiplicativeExpression
    :   unaryExpression ((Star|Slash|Percent) unaryExpression)*
    ;

additiveExpression
    :   multiplicativeExpression ((Plus|Minus) multiplicativeExpression)*
    ;

relationalExpression
    :    additiveExpression ((Greater|Less|Greaterequal|Lessequal) additiveExpression)*
    ;

equalityExpression
    :    relationalExpression ((Equalequal|Exclaimequal) relationalExpression)*
    ;

logicAndExpression
    :   equalityExpression ((Ampamp) equalityExpression)*
    ;

logicOrExpression
    :   logicAndExpression ((Pipepipe) logicAndExpression)*
    ;

conditionalExpression
    :   logicOrExpression
    ;

assignmentExpression
    :   conditionalExpression
    |   unaryExpression Equal assignmentExpression
    ;

expression
    :   assignmentExpression (Comma assignmentExpression)*
    ;

declaration
    :   declarationSpecifiers initDeclaratorList? Semi
    ;

declarationSpecifiers
    :   declarationSpecifier+
    ;

declarationSpecifier
    :   typeSpecifier
    ;

initDeclaratorList
    :   initDeclarator (Comma initDeclarator)*
    ;

initDeclarator
    :   declarator (Equal initializer)?
    ;


typeSpecifier
    :   Int | Const | Void
    ;


declarator
    :   directDeclarator
    ;

directDeclarator
    :   Identifier
    |   directDeclarator LeftBracket assignmentExpression? RightBracket
    |   directDeclarator LeftParen parameterList? RightParen
    ;
//  Identifier (LeftBracket assignmentExpression? RightBracket | LeftParen parameterTypeList? RightParen)* 与上述文法等价

parameterList
    :   parameterDeclaration (Comma parameterDeclaration)*
    ;

parameterDeclaration
    :   declarationSpecifiers declarator
    ;

identifierList
    :   Identifier (Comma Identifier)*
    ;

initializer
    :   assignmentExpression
    |   LeftBrace initializerList? Comma? RightBrace
    ;

initializerList
    // :   designation? initializer (Comma designation? initializer)*
    :   initializer (Comma initializer)*
    ;

statement
    :   compoundStatement
    |   expressionStatement
    |   jumpStatement
    |   ifStatement
    |   whileStatement
    ;

compoundStatement
    :   LeftBrace blockItemList? RightBrace
    ;

blockItemList
    :   blockItem+
    ;

blockItem
    :   statement
    |   declaration
    ;

expressionStatement
    :   expression? Semi
    ;



jumpStatement
    :   Return expression? Semi
    |   Break Semi
    |   Continue Semi
    ;

ifStatement
    :   If LeftParen expression RightParen statement
    |   If LeftParen expression RightParen statement Else statement
    ;

whileStatement
    :   While LeftParen expression RightParen statement
    ;

compilationUnit
    :   translationUnit? EOF
    ;

translationUnit
    :   externalDeclaration+
    ;

externalDeclaration
    :   functionDefinition
    |   declaration
    ;

functionDefinition
    : declarationSpecifiers directDeclarator compoundStatement
    ;

