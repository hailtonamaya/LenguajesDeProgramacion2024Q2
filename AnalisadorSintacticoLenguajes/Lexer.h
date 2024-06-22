#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <algorithm>

enum class TokenType {
    Number,
    Identifier,
    Equals,
    OpenParen,
    CloseParen,
    BinaryOperator,
    Let,
    Comma,
    StringLiteral,
    Keyword,
    DoubleColon,
    Unknown,
    Comment,
};

struct Token {
    std::string value;
    TokenType type;
};

class Lexer {
public:
    Lexer(const std::string &sourceCode);
    void tokenize();
    const std::vector<Token>& getTokens() const;
    void printTokens() const;

private:
    std::vector<std::string> splitString(const std::string &sourceCode);
    bool isNumber(const std::string &str);
    bool isAlpha(const std::string &str);
    bool isAlnum(const std::string &str);
    bool isSkippable(char ch);
    Token createToken(const std::string &value, TokenType type);
    std::string shift(std::vector<std::string> &src);

    std::string sourceCode;
    std::vector<Token> tokens;
};


