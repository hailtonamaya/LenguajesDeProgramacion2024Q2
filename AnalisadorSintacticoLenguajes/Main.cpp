#include "Parser.h"

int main() {
    std::string filename;
    std::cout << "Ingrese el nombre del archivo: ";
    std::cin >> filename;

    std::ifstream file(filename);

    if (!file) {
        std::cerr << "No se pudo abrir el archivo: " << filename << std::endl;
        return 1;
    }

    std::stringstream buffer;
    buffer << file.rdbuf();
    std::string sourceCode = buffer.str();

    Lexer lexer(sourceCode);
    lexer.tokenize();

    const std::vector<Token>& tokens = lexer.getTokens();
    Parser parser(tokens);
    parser.programa();

    return 0;
}
