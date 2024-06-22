lexer grammar SYsULexer;

Int : 'int';
Return : 'return';
Const : 'const';
If : 'if';
Else : 'else';
While : 'while';
Void : 'void';
Break : 'break';
Continue : 'continue';
Greater : '>';
Less : '<';
Lessequal : '<=';
Greaterequal : '>=';
Equalequal : '==';
Exclaimequal : '!=';
Pipepipe : '||';
Ampamp : '&&';
Exclaim : '!';

LeftParen : '(';
RightParen : ')';
LeftBracket : '[';
RightBracket : ']';
LeftBrace : '{';
RightBrace : '}';

Plus : '+';
Minus : '-';
Star : '*';
Slash : '/';
Percent : '%';

Semi : ';';
Comma : ',';

Equal : '=';

Identifier
    :   IdentifierNondigit
        (   IdentifierNondigit
        |   Digit
        )*
    ;

fragment
IdentifierNondigit
    :   Nondigit
    ;

fragment
Nondigit
    :   [a-zA-Z_]
    ;

fragment
Digit
    :   [0-9]
    ;

Constant
    :   IntegerConstant
    ;

fragment
IntegerConstant
    :   DecimalConstant
    |   OctalConstant
    |   HexadecimalConstant
    ;

fragment
DecimalConstant
    :   NonzeroDigit Digit*
    ;

fragment
OctalConstant
    :   '0' OctalDigit*
    ;
    
//'0' OctalDigit* {IS}?
// '0' [1-9] [0-9]*
//IS    ((u|U)|(u|U)?(l|L|ll|LL)|(l|L|ll|LL)(u|U))

fragment
HexadecimalConstant
    :   '0x' [1-9a-fA-F] HexadecimalDigit*
    ;

fragment
NonzeroDigit
    :   [1-9]
    ;

fragment
OctalDigit
    :   [0-7]
    ;

fragment
HexadecimalDigit
    :   [0-9a-fA-F]
    ;


// 预处理信息处理，可以从预处理信息中获得文件名以及行号
// 预处理信息前面的数组即行号
LineAfterPreprocessing
    :   '#' Whitespace* ~[\r\n]*
        
    ;

// LineAfterPreprocessing
//     :   '#' Whitespace* ~[\r\n]*
//         -> skip
//     ;

Whitespace
    :   [ \t]+
        
    ;

// 换行符号，可以利用这个信息来更新行号
Newline
    :   (   '\r' '\n'?
        |   '\n'
        )
        
    ;

