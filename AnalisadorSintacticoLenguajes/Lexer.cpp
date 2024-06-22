#include "Lexer.h"

Lexer::Lexer(const std::string &sourceCode) : sourceCode(sourceCode) {}

void Lexer::tokenize() {
    std::vector<std::string> src = splitString(sourceCode);

    while (!src.empty()) {
        if (src.front() == "(") {
            tokens.push_back(createToken(shift(src), TokenType::OpenParen));
        } else if (src.front() == ")") {
            tokens.push_back(createToken(shift(src), TokenType::CloseParen));
        } else if (src.front() == "=") {
            tokens.push_back(createToken(shift(src), TokenType::Equals));
        } else if (src.front() == "+" || src.front() == "-" || src.front() == "*" || src.front() == "/") {
            tokens.push_back(createToken(shift(src), TokenType::BinaryOperator));
        } else if (src.front() == ",") {
            tokens.push_back(createToken(shift(src), TokenType::Comma));
        } else if (src.front()[0] == '"') {
            std::string stringLiteral;
            if (src.front().size() == 3 && src.front()[0] == '"' && src.front()[2] == '"') {
                stringLiteral = shift(src);
            } else {
                do {
                    stringLiteral += shift(src);
                } while (!src.empty() && src.front().back() != '"');
                if (!src.empty()) {
                    stringLiteral += shift(src);
                }
            }
            tokens.push_back(createToken(stringLiteral, TokenType::StringLiteral));
        } else if (src.front()[0] == '!') {
            tokens.push_back(createToken(shift(src), TokenType::Comment));
        } else if (isNumber(src.front())) {
            tokens.push_back(createToken(shift(src), TokenType::Number));
        } else if (isAlnum(src.front())) {
            std::string ident = shift(src);
            if (ident == "let" || ident == "program" || ident == "end" || ident == "implicit" || ident == "none" || ident == "complex" || ident == "print" || ident == "integer") {
                tokens.push_back(createToken(ident, TokenType::Keyword));
            } else {
                tokens.push_back(createToken(ident, TokenType::Identifier));
            }
        } else if (src.front() == "::") {
            tokens.push_back(createToken(shift(src), TokenType::DoubleColon));
        } else if (isSkippable(src.front()[0])) {
            shift(src);
        } else {
            tokens.push_back(createToken(shift(src), TokenType::Unknown));
        }
    }
}

const std::vector<Token>& Lexer::getTokens() const {
    return tokens;
}

void Lexer::printTokens() const {
    for (const auto &token : tokens) {
        std::cout << "Value: " << token.value << "   Type: " << static_cast<int>(token.type) << std::endl;
    }
}

std::vector<std::string> Lexer::splitString(const std::string &sourceCode) {
    std::vector<std::string> words;
    std::string word;

    for (char ch : sourceCode) {
        if (std::isalnum(ch) || ch == '_' || ch == '"' || ch == '.' || ch == '!') {
            word += ch;
        } else {
            if (!word.empty()) {
                words.push_back(word);
                word.clear();
            }
            if (!std::isspace(ch)) {
                words.push_back(std::string(1, ch));
            }
        }
    }

    if (!word.empty()) {
        words.push_back(word);
    }

    for (size_t i = 0; i < words.size() - 1; ++i) {
        if (words[i] == ":" && words[i + 1] == ":") {
            words[i] = "::";
            words.erase(words.begin() + i + 1);
        }
    }

    return words;
}

bool Lexer::isNumber(const std::string &str) {
    return !str.empty() && std::all_of(str.begin(), str.end(), [](char c){ return std::isdigit(c) || c == '.'; });
}

bool Lexer::isAlpha(const std::string &str) {
    return !str.empty() && std::all_of(str.begin(), str.end(), ::isalpha);
}

bool Lexer::isAlnum(const std::string &str) {
    return !str.empty() && std::all_of(str.begin(), str.end(), [](char c){ return std::isalnum(c) || c == '_'; });
}

bool Lexer::isSkippable(char ch) {
    return std::isspace(ch);
}

Token Lexer::createToken(const std::string &value, TokenType type) {
    return {value, type};
}

std::string Lexer::shift(std::vector<std::string> &src) {
    std::string first = src.front();
    src.erase(src.begin());
    return first;
}
