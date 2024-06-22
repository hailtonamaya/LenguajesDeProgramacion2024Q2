#ifndef VARIABLES_H
#define VARIABLES_H

#include <iostream>
#include <sstream>
#include <string>
#include <vector>
#include <unordered_map>
#include <regex>
#include <algorithm>
#include "Constantes.h"
#include <gtk/gtk.h>

using namespace std;

struct Variable {
    string nombre;
    double valor;
};

class Variables {
public:
    Variables(Constantes constantesFile, GtkWidget *mainWindow)
        : constantes(constantesFile), mainWindow(mainWindow) {}


    void show_alert(const string &message) const {
        GtkWidget *dialog;
        dialog = gtk_message_dialog_new(GTK_WINDOW(mainWindow),
                                        GTK_DIALOG_DESTROY_WITH_PARENT,
                                        GTK_MESSAGE_INFO,
                                        GTK_BUTTONS_OK,
                                        "%s", message.c_str());
        gtk_window_set_title(GTK_WINDOW(dialog), "Alerta");
        gtk_dialog_run(GTK_DIALOG(dialog));
        gtk_widget_destroy(dialog);
    }

    string reemplazarVariables(const string &expresion) const {
        string resultado = expresion;
        for (const auto &var : variables) {
            regex varRegex("\\b" + var.nombre + "\\b");
            resultado = regex_replace(resultado, varRegex, to_string(var.valor));
        }
        return resultado;
    }

    void insertarVariable(const string &cadena) {
        string linea = cadena;

        linea.erase(remove_if(linea.begin(), linea.end(), ::isspace), linea.end());

        istringstream stream(linea);
        string nombre;
        double valor;

        if (getline(stream, nombre, '=') && stream >> valor) {

            if (!constantes.validarNombre(nombre)) {
                stringstream ss;
                ss << "Error: El nombre '" << nombre << "' está definido como constante y no se puede modificar.";
                show_alert(ss.str());
                return;
            }


            auto it = find_if(variables.begin(), variables.end(),
                                   [&nombre](const Variable &v) { return v.nombre == nombre; });

            if (it != variables.end()) {

                it->valor = valor;
            } else {

                Variable var = {nombre, valor};
                variables.push_back(var);
            }
        } else {
            stringstream ss;
            ss << "Formato de cadena incorrecto: " << cadena;
            show_alert(ss.str());
        }
    }

    void imprimirVariables() const {
        for (const auto &var : variables) {
            cout << "Nombre: " << var.nombre << ", Valor: " << var.valor << endl;
        }
    }

    const char *obtenerCadenaDeVariables() const {
        stringstream ss;
        for (const auto &var : variables) {
            ss << var.nombre << "=" << var.valor << "\n";
        }
        cadenaDeVariables = ss.str();
        return cadenaDeVariables.c_str();
    }

private:
    vector<Variable> variables;
    unordered_map<string, double> variablesMap;
    mutable string cadenaDeVariables;

    Constantes constantes;
    GtkWidget *mainWindow;
};

#endif // VARIABLES_H
