#ifndef EVALUADOR_H
#define EVALUADOR_H

#include <iostream>
#include <stack>
#include <sstream>
#include <string>
#include <map>
#include <cctype>
#include <gtk/gtk.h>

using namespace std;

bool esOperador(char c);
int precedencia(char c);

class Evaluador {
public:
    Evaluador( GtkWidget *mainWindow);
    Evaluador( );

    void showAlert(const string& message);
    void imprimirEstado(const string& resultado, const stack<char>& pila);
    string infijaAPostfija(const string& infija);
    void imprimirEstadoPila(const stack<double>& pila);
    double evaluarPostfija(const string& expresion);
    bool evaluarInfija(const string& expresion);

private:

    GtkWidget *mainWindow;
    bool MostrarInterfaz = false;
};

#endif // EVALUADOR_H
