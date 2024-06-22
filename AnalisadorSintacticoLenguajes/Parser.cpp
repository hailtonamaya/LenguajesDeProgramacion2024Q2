#include "Parser.h"
#include <iostream>

Parser::Parser(const std::vector<Token>& tokens) : tokens(tokens), currentTokenIndex(0) {}

void Parser::programa() {
    Token token = getToken();
    if (token.value == "program") {
        identifier();
        declarationSection();
        executableSection();

        token = getToken();
        Token token2 = getToken();
        if (token.value == "end" && token2.value == "program") {
            identifier();
        } else {
            std::cout << "Error: Se esperaba 'end program'" << std::endl;
            return;
        }
        std::cout << "Programa compilado con exito" << std::endl;
        return;
    } else {
        std::cout << "Error: Se esperaba 'program'" << std::endl;
        return;
    }
}

void Parser::identifier() {
    Token token = getToken();
    if (token.type != TokenType::Identifier) {
        std::cout << "Se esperaba un identificador" << std::endl; return;
    }
}

void Parser::declarationSection() {
    Token token = getToken();
    Token token2 = getToken();

    if (token.value != "implicit" || token2.value != "none") {
        std::cout << "Se espera un 'implicit none'" << std::endl;return;
    }
}

void Parser::executableSection() {
    while (hasMoreTokens() && getNextToken().value != "end" && getNextToken1().value != "program"){
        Token token = getNextToken();
        if (token.type == TokenType::Identifier || token.value == "do" || token.value == "print" || token.type == TokenType::Comment || token.type == TokenType::Keyword) {
            executableConstruct();
        } else {
            break;
        }
    }
}

void Parser::executableConstruct() {
    Token token = getNextToken();
    if (token.type == TokenType::Identifier) {
        actionStmt();
    } else if (token.value == "do") {
        doConstruct();
    } else if (token.value == "print") {
        printStmt();
    } else if (token.type == TokenType::Comment) {
        comment();
    } else if(token.type == TokenType::Keyword && token.value == "print") {
        printStmt();
    }else if(token.type == TokenType::Keyword) {
        declarationConstruct();
    } else {
        std::cout << "Error: Se esperaba un identificador, do, print o !" << std::endl;
        return;
    }
}

void Parser::declarationConstruct() {
    declarationTypeSpec();
    Token token = getToken();
    if (token.value == "::") {
        entityDecList();
    } else {
        std::cout << "Se esperaba un '::'" << std::endl;
        return;
    }
}

void Parser::entityDecList() {
    entityDecl();
    while (hasMoreTokens() && getNextToken().value == ",") {
        getToken(); 
        entityDecl();
    }
}

void Parser::entityDecl() {
    objectName();
}

void Parser::arraySpec() {
    inLiteralConstant();
    while (hasMoreTokens() && getNextToken().value == ",") {
        getToken(); 
        inLiteralConstant();
    }
}

void Parser::inLiteralConstant() {
    Token token = getToken();
    if (token.type != TokenType::Number) {
        std::cout << "Se esperaba un numero" << std::endl;
        return;
    }
}

void Parser::declarationTypeSpec() {
    Token token = getToken();
    if (token.value != "integer" && token.value != "complex") {
        std::cout << "Se esperaba un 'integer' o 'complex'" << std::endl;
        return;
    }
}

void Parser::actionStmt() {
    objectName();
    Token token = getToken();
    if (token.value == "=") {
        expr();
    } else {
        std::cout << "Se espera un '='" << std::endl;
        return;
    }
}

void Parser::doConstruct() {
    getToken(); 
    objectName();
    Token token = getToken();
    if (token.value == "=") {
        intLiteralConstant();
        token = getNextToken();
        if (token.value == ",") {
            getToken(); 
            expr();
        }
    } else {
        std::cout << "Se esperaba un '='" << std::endl;
        return;
    }
    token = getToken();
    Token token2 = getToken();
    if (token.value != "end" || token2.value != "do") {
        std::cout << "Se esperaba un 'end do'" << std::endl;
        return;
    }
}

void Parser::printStmt() {
    getToken(); 
    getToken(); 
    if (getToken().value == ",") {
        charLiteralConstant();
        while (hasMoreTokens() && getNextToken().value == ",") {
            getToken(); 
            if(getNextToken().type == TokenType::BinaryOperator){
                getToken();
            }else if(getNextToken().type == TokenType::StringLiteral){
                charLiteralConstant();
            }else{
                expr();
            }
            
        }
    } else {
        std::cout << "Se esperaba un ',' despues de '*'" << std::endl;
        return;
    }
}

void Parser::comment() {
    Token token  = getToken();
    if(token.type == TokenType::Comment) {
        anyText();
    } else {
        std::cout << "Se esperaba un comentario" << std::endl;
        return;
    }
}

void Parser::expr() {
        if (getNextToken().type == TokenType::Number) {
            intLiteralConstant();
        } else if (getNextToken().type == TokenType::Identifier) {
            objectName();
            if(getNextToken().type == TokenType::OpenParen && getNextToken1().type == TokenType::Identifier && getNextToken2().type == TokenType::CloseParen)
                parameter();
        } else if (getNextToken().type == TokenType::Identifier && getNextToken1().value == "(") {
            arrayElement();
        }else if(getNextToken().type == TokenType::OpenParen && getNextToken1().type == TokenType::Identifier && getNextToken2().type == TokenType::CloseParen){
            parameter();
        } else if (getNextToken().type == TokenType::OpenParen) {
            complexLiteralConstant();
        }
        
        if (getNextToken().type == TokenType::BinaryOperator) {
            while(getNextToken().type == TokenType::BinaryOperator) {
                getToken();
                expr();
            }
        }
   
}

void Parser::parameter() {
    Token token = getToken(); 
    if(token.value == "(") {
        objectName();
        token = getToken();
        if(token.value != ")") {
            std::cout << "Se esperaba un ')'" << std::endl;
            return;
        }
    } else {
        std::cout << "Se esperaba un '(' " << std::endl;
        return;
    }
}

void Parser::objectName() {
    Token token = getToken();
    if (token.type != TokenType::Identifier) {
        std::cout << "Se esperaba un identificador" << std::endl;
        return;
    }
}

void Parser::intLiteralConstant() {
    Token token = getToken();
    if (token.type != TokenType::Number) {
        std::cout << "Se esperaba un entero" << std::endl;
        return;
    }
}

void Parser::arrayElement() {
    objectName();
    if (getToken().value == "(") {
        intLiteralConstant();
        if (getToken().value != ")") {
            std::cout << "Se esperaba un ')'" << std::endl;
            return;
        }
    } else {
        std::cout << "Se esperaba un '('" << std::endl;
        return;
    }
}

void Parser::complexLiteralConstant() {
    if (getToken().value == "(") {
        realPart();
        if (getToken().value == ",") {
            imagPart();
            if (getToken().value != ")") {
                std::cout << "Se esperaba un ')'" << std::endl;
                return;
            }
        } else {
            std::cout << "Se esperaba una ','" << std::endl;
            return;
        }
    } else {
        std::cout << "Se esperaba un '(' " << std::endl;
        return;
    }
}

void Parser::realPart() {
    Token token = getNextToken();
    if(token.value == "-") {
        getToken();
        token = getToken();
    }else{
        token = getToken();
    }
    if (token.type != TokenType::Number) {
        std::cout << "Se esperaba un numero" << std::endl;
        return;
    }
}

void Parser::imagPart() {
    Token token = getNextToken();
    if(token.value == "-") {
        getToken();
        token = getToken();
    }else{
        token = getToken();
    }
    if (token.type != TokenType::Number) {
        std::cout << "Se esperaba un numero" << std::endl;
        return;
    }
}

void Parser::anyText() {
    Token token = getToken();
    char lastChar = '\0';
    while (token.type == TokenType::Number || token.type == TokenType::Identifier || token.type == TokenType::Equals || token.type == TokenType::OpenParen || token.type == TokenType::CloseParen || token.type == TokenType::BinaryOperator || token.type == TokenType::Comma || token.type == TokenType::Comment) {
        token = getToken(); 
        std::cout << token.value << std::endl;
        for (char c : token.value) {
            if (c == '\n' && lastChar == '!') {
                break;
            }
            lastChar = c;
        }
    }
}

void Parser::charLiteralConstant() {
    Token token = getToken();
    if (token.type != TokenType::StringLiteral) {
        std::cout << "Se esperaba un texto" << std::endl;
        return;
    }
}

Token Parser::getToken() {
    if (currentTokenIndex < tokens.size()) {
        return tokens[currentTokenIndex++];
    } else {
        return Token{ "", TokenType::Unknown }; 
    }
}

Token Parser::getNextToken() {
    if (currentTokenIndex < tokens.size()) {
        return tokens[currentTokenIndex];
    } else {
        return Token{ "", TokenType::Unknown }; 
    }
}

Token Parser::getNextToken1() {
    if (currentTokenIndex + 1 < tokens.size()) {
        return tokens[currentTokenIndex + 1];
    } else {
        return Token{ "", TokenType::Unknown }; 
    }
}

Token Parser::getNextToken2() {
    if (currentTokenIndex + 2 < tokens.size()) {
        return tokens[currentTokenIndex + 2];
    } else {
        return Token{ "", TokenType::Unknown }; 
    }
}

bool Parser::hasMoreTokens() const {
    return currentTokenIndex < tokens.size();
}
