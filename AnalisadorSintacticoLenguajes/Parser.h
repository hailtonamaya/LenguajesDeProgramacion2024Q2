#include "Lexer.h"
#include <vector>

class Parser {
public:
    Parser(const std::vector<Token>& tokens);
    void programa();
    
private:
    void identifier();
    void declarationSection();
    void executableSection();
    void executableConstruct();
    void actionStmt();
    void doConstruct();
    void printStmt();
    void comment();
    void objectName();
    void intLiteralConstant();
    void arrayElement();
    void anyText();
    void expr();
    void complexLiteralConstant();
    void charLiteralConstant();
    void realPart();
    void imagPart();
    void arraySpec();
    void inLiteralConstant();
    void entityDecl();
    void entityDecList();
    void declarationConstruct();
    void declarationTypeSpec();
    void parameter();
    Token getToken();
    Token getNextToken();
    Token getNextToken1();
    Token getNextToken2();
    bool hasMoreTokens() const;

    std::vector<Token> tokens;
    Token currentToken;
    size_t currentTokenIndex;
};
