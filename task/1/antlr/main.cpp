#include "SYsULexer.h" // 确保这里的头文件名与您生成的词法分析器匹配
#include <fstream>
#include <iostream>
#include <unordered_map>

int lineNum = 0;                // 行号
std::string fileLocation = "";  // 文件位置
bool startOfLine = false;       // 是否为一行的第一个非 whitespace token
bool leadingSpace = false;      // token 前是否有 whitespqce

// 映射定义，将ANTLR的tokenTypeName映射到clang的格式
std::unordered_map<std::string, std::string> tokenTypeMapping = {
  { "Int", "int" },
  { "Identifier", "identifier" },
  { "LeftParen", "l_paren" },
  { "RightParen", "r_paren" },
  { "RightBrace", "r_brace" },
  { "LeftBrace", "l_brace" },
  { "LeftBracket", "l_square" },
  { "RightBracket", "r_square" },
  { "Constant", "numeric_constant" },
  { "Return", "return" },
  { "Semi", "semi" },
  { "EOF", "eof" },
  { "Equal", "equal" },
  { "Plus", "plus" },
  { "Star", "star" },
  { "Slash", "slash" },
  { "Minus", "minus" },
  { "Percent", "percent" },
  { "Comma", "comma" },
  { "If", "if" },
  { "Else", "else" },
  { "While", "while" },
  { "Const", "const" },
  { "Void", "void" },
  { "Break", "break" },
  { "Continue", "continue" }, 
  { "Greater", "greater" },
  { "Less", "less" },
  { "Lessequal", "lessequal" },
  { "Greaterequal", "greaterequal" },
  { "Equalequal", "equalequal" },
  { "Exclaimequal", "exclaimequal" },
  { "Pipepipe", "pipepipe" },
  { "Ampamp", "ampamp" },
  { "Exclaim", "exclaim" },
  { "LineAfterPreprocessing", "LineAfterPreprocessing" },
  { "Whitespace", "Whitespace" },
  { "Newline", "Newline" },

  // 在这里继续添加其他映射
};

void
print_token(const antlr4::Token* token,
            const antlr4::CommonTokenStream& tokens,
            std::ofstream& outFile,
            const antlr4::Lexer& lexer)
{
  auto& vocabulary = lexer.getVocabulary();

  auto tokenTypeName =
    std::string(vocabulary.getSymbolicName(token->getType()));

  if (tokenTypeName.empty())
    tokenTypeName = "<UNKNOWN>"; // 处理可能的空字符串情况

  if (tokenTypeMapping.find(tokenTypeName) != tokenTypeMapping.end()) {
    tokenTypeName = tokenTypeMapping[tokenTypeName];
  }

  if (tokenTypeName == "LineAfterPreprocessing") {
    std::string s = token->getText();
    fileLocation = s.substr(s.find('"')+1, s.rfind('"')-s.find('"')-1 );
    lineNum = 0;
    for (int i=2;s[i]!=' ';i++)   // 计算文件的起始行号
    {
      lineNum = lineNum*10 + s[i] - 48;
    }

    lineNum--;  // Newline会++
  }

  if (tokenTypeName == "Newline") {   // 更新信息
    startOfLine = true;
    leadingSpace = false;
    lineNum++;
  } else if (tokenTypeName == "Whitespace") {
    leadingSpace = true;
  }

  if (tokenTypeName == "LineAfterPreprocessing" || tokenTypeName == "Whitespace" || tokenTypeName == "Newline") {
    return;
  }

  int col = token->getCharPositionInLine() + 1;   // 从1开始
  
  std::string locInfo = "Loc=<" + fileLocation + ":" + std::to_string(lineNum) +  ":" + std::to_string(col) + ">";


  if (token->getText() != "<EOF>")
    outFile << tokenTypeName << " '" << token->getText() << "'";
  else
    outFile << tokenTypeName << " '"
            << "'";
  
  if (startOfLine && leadingSpace) {
    outFile << "\t [StartOfLine] [LeadingSpace]";
  }else if (startOfLine)
    outFile << "\t [StartOfLine]";
   else if (leadingSpace)
    outFile << "\t [LeadingSpace]";
   else
    outFile << "\t";

  outFile << "\t" << locInfo << std::endl;

  startOfLine = false;
  leadingSpace = false;
}

int
main(int argc, char* argv[])
{
  if (argc != 3) {
    std::cout << "Usage: " << argv[0] << " <input> <output>\n";
    return -1;
  }

  std::ifstream inFile(argv[1]);
  if (!inFile) {
    std::cout << "Error: unable to open input file: " << argv[1] << '\n';
    return -2;
  }

  std::ofstream outFile(argv[2]);
  if (!outFile) {
    std::cout << "Error: unable to open output file: " << argv[2] << '\n';
    return -3;
  }

  std::cout << "程序 '" << argv[0] << std::endl;
  std::cout << "输入 '" << argv[1] << std::endl;
  std::cout << "输出 '" << argv[2] << std::endl;

  antlr4::ANTLRInputStream input(inFile);
  SYsULexer lexer(&input);

  antlr4::CommonTokenStream tokens(&lexer);
  tokens.fill();

  for (auto&& token : tokens.getTokens()) {
    print_token(token, tokens, outFile, lexer);
  }
}
