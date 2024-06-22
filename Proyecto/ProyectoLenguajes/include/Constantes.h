#ifndef CONSTANTES_H
#define CONSTANTES_H

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <regex>
#include <algorithm>

using namespace std;

struct Constante {
    string nombre;
    double valor;
};

class Constantes {
public:
    Constantes();
    virtual ~Constantes();

    string obtenerCadenaDeConstantes() const;
    string reemplazarConstantes(const string &expresion) const;
    bool validarNombre(const string &nombre) const;
void imprimirConstantes() const;
private:
    vector<Constante> constantes;
    unordered_map<string, double> constantesMap;

    void leerArchivo(const string &filename);
};

Constantes::Constantes() {

   try {
        leerArchivo("constantes");
    } catch (const exception &ex) {
        cerr << "Error al inicializar Constantes: " << ex.what() << endl;

    }
}

Constantes::~Constantes() {}

void Constantes::leerArchivo(const string &filename) {
    ifstream file(filename);
    if (!file.is_open()) {
        throw runtime_error("No se pudo abrir el archivo: " + filename);
    }

    string line;
    while (getline(file, line)) {

        line.erase(remove_if(line.begin(), line.end(), ::isspace), line.end());

        if (line.empty() || line[0] == '#') {
            continue;
        }


        auto equalPos = line.find('=');
        if (equalPos == string::npos) {
            cerr << "Error de formato en la línea: " << line << endl;
            continue;
        }

        string nombre = line.substr(0, equalPos);
        string valorStr = line.substr(equalPos + 1);
        double valor = stod(valorStr);


        auto it = find_if(constantes.begin(), constantes.end(),
                               [&nombre](const Constante &c) { return c.nombre == nombre; });

        if (it != constantes.end()) {
            cerr << "Advertencia: La constante '" << nombre << "' ya está definida." << endl;
            continue;
        }


        Constante constante = {nombre, valor};
        constantes.push_back(constante);
        constantesMap[nombre] = valor;
    }

    file.close();
}

string Constantes::obtenerCadenaDeConstantes() const {
    stringstream ss;
    for (const auto &constante : constantes) {
        ss << constante.nombre << "=" << constante.valor << "\n";
    }
    return ss.str();
}

string Constantes::reemplazarConstantes(const string &expresion) const {
    string resultado = expresion;
    for (const auto &constante : constantes) {
        regex constRegex("\\b" + constante.nombre + "\\b");
        resultado = regex_replace(resultado, constRegex, to_string(constante.valor));
    }
    return resultado;
}

bool Constantes::validarNombre(const string &nombre) const {
    auto it = find_if(constantes.begin(), constantes.end(),
                           [&nombre](const Constante &c) { return c.nombre == nombre; });

    return it == constantes.end();
}


void Constantes::imprimirConstantes() const {
    for (const auto &constante : constantes) {
        cout << constante.nombre << " = " << constante.valor << endl;
    }
}




#endif // CONSTANTES_H
