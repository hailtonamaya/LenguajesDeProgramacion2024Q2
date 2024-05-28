#include <iostream>
#include <fstream>
#include <string>
#include <regex>

using namespace std;

bool isLineOfCode(string line) {
    // Quitar las lineas que tengan pleca con doble comillas 
    line = regex_replace(line, regex("\".*?\""), "");
    line = regex_replace(line, regex("'.*?'"), "");
    
    // Quitar los comentarios de una linea
    line = regex_replace(line, regex("//.*"), "");
    
    // Quitar los comentarios de bloque
    while (line.find("/*") != string::npos && line.find("*/") != string::npos) {
        size_t inicio = line.find("/*");
        size_t fin = line.find("*/") + 2;
        line = line.substr(0, inicio) + line.substr(fin);
    }

    // Quitar los comentarios de bloque que empiezan en una linea y terminan en otra
    if (line.find("/*") != string::npos) {
        line = line.substr(0, line.find("/*"));
    }
    
    // Enviar si la linea no esta vacia
    return !line.empty() && line.find_first_not_of(" \t\n\v\f\r") != string::npos;
}

int countLinesOfCode(string filePath) {
    bool inBlockComment = false;
    int linesOfCode = 0;
    
    ifstream file(filePath);
    if (!file.is_open()) {
        cerr << "Failed to open the file." << endl;
        return -1;
    }
    
    string line;
    while (getline(file, line)) {
        string strippedLine = line;
        strippedLine.erase(0, strippedLine.find_first_not_of(" \t\n\v\f\r")); // trim leading spaces
        
        if (inBlockComment) {
            if (strippedLine.find("*/") != string::npos) {
                inBlockComment = false;
                strippedLine = strippedLine.substr(strippedLine.find("*/") + 2);
            } 
        }
        
        if (!inBlockComment) {
            if (strippedLine.find("/*") != string::npos && strippedLine.find("*/") == string::npos) {
                inBlockComment = true;
                strippedLine = strippedLine.substr(0, strippedLine.find("/*"));
            }
            
            if (isLineOfCode(strippedLine)) {
                linesOfCode += 1;
            }
        }
    }
    
    file.close();
    return linesOfCode;
}

int main() {
    string filePath = "archivo.java";
    int linesOfCode = countLinesOfCode(filePath);
    
    if (linesOfCode != -1) {
        cout << "Numero de lineas de codigo: " << linesOfCode << endl;
    }
    
    return 0;
}
