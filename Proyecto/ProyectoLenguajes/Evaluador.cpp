#include "Evaluador.h"
#include <stack>
#include <sstream>
#include <string>
#include <cctype>
#include <cmath>

using namespace std;

Evaluador::Evaluador( GtkWidget *mainWindow)
    :  mainWindow(mainWindow), MostrarInterfaz(true) {
MostrarInterfaz=true;
}

Evaluador::Evaluador( )
   : mainWindow(nullptr),MostrarInterfaz(false) {
MostrarInterfaz=false;
}

void Evaluador::showAlert(const string& message) {


if(MostrarInterfaz){
    GtkWidget *dialog;
    dialog = gtk_message_dialog_new(GTK_WINDOW(mainWindow),
                                    GTK_DIALOG_MODAL,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    "%s",
                                    message.c_str());
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
    }
}

int precedencia(char op) {
    switch (op) {
    case '+':
    case '-':
        return 1;
    case '*':
    case '/':
    case '%':
        return 2;
    case '^':
        return 3;
    case '(':
    case ')':
        return 0;
    default:
        return -1;
    }
}

bool esOperador(char c) {
    return c == '+' || c == '-' || c == '*' || c == '/'|| c == '%'|| c == '^';
}

void Evaluador::imprimirEstado(const string& resultado, const stack<char>& pila) {
  string mensaje = "Resultado => " + resultado + "\nPila => [ ";


    stack<char> pilaCopia = pila;
    while (!pilaCopia.empty()) {
        mensaje += pilaCopia.top();
        mensaje += " ";
        pilaCopia.pop();
    }
    mensaje += "]";
   if(MostrarInterfaz){ cout << mensaje << endl;}
   // showAlert(mensaje);
}

string Evaluador::infijaAPostfija(const string& infija) {
    //showAlert("-----Convirtiendo de infija a postfija-----");
   if(MostrarInterfaz){ cout << "-----Convirtiendo de infija a postfija-----" << endl;}
    string postfija;
    stack<char> pila;
    string numero;

    for (char ch : infija) {
        if (isspace(ch)) {
            continue;
        } else if (isdigit(ch) || ch == '.') {
            numero += ch;
        } else {
            if (!numero.empty()) {
                if (!postfija.empty()) {
                    postfija += ", ";
                }
                postfija += numero;
                numero.clear();
              if(MostrarInterfaz){  imprimirEstado(postfija, pila);}
            }

            if (ch == '(') {
                pila.push(ch);
            } else if (ch == ')') {
                while (!pila.empty() && pila.top() != '(') {
                    if (!postfija.empty()) {
                        postfija += ", ";
                    }
                    postfija += pila.top();
                    pila.pop();
                  if(MostrarInterfaz){  imprimirEstado(postfija, pila);}
                }
                pila.pop();
            } else {
                while (!pila.empty() && precedencia(pila.top()) >= precedencia(ch)) {
                    if (!postfija.empty()) {
                        postfija += ", ";
                    }
                    postfija += pila.top();
                    pila.pop();
               if(MostrarInterfaz){     imprimirEstado(postfija, pila);}
                }
                pila.push(ch);
            }
           if(MostrarInterfaz){ imprimirEstado(postfija, pila);}
        }
    }

    if (!numero.empty()) {
        if (!postfija.empty()) {
            postfija += ", ";
        }
        postfija += numero;
    if(MostrarInterfaz){    imprimirEstado(postfija, pila);}
    }

    while (!pila.empty()) {
        if (!postfija.empty()) {
            postfija += ", ";
        }
        postfija += pila.top();
        pila.pop();
    if(MostrarInterfaz){    imprimirEstado(postfija, pila);}
    }

    return postfija;
}

void Evaluador::imprimirEstadoPila(const stack<double>& pila) {
    stack<double> pilaCopia = pila;
    ostringstream oss;
    oss << "Pila => [ ";
    while (!pilaCopia.empty()) {
        oss << pilaCopia.top() << " ";
        pilaCopia.pop();
    }
    oss << "]";
    string mensaje = oss.str();
 if(MostrarInterfaz){   cout << mensaje << endl;}
    //showAlert(mensaje);
}

double Evaluador::evaluarPostfija(const string& expresion) {
    //showAlert("-----Evaluando expresion postfija-----");
if(MostrarInterfaz){    cout << "-----Evaluando expresion postfija-----" << endl;}
    stack<double> pila;
    stringstream ss(expresion);
    string token;

    while (getline(ss, token, ',')) {
        token.erase(0, token.find_first_not_of(" "));
        token.erase(token.find_last_not_of(" ") + 1);

        if (!token.empty() && (isdigit(token[0]) || token[0] == '.' || (token[0] == '-' && token.length() > 1))) {
            pila.push(stod(token));
           if(MostrarInterfaz){ imprimirEstadoPila(pila);}
        } else if (!token.empty() && esOperador(token[0])) {
            double operand2 = pila.top(); pila.pop();
            double operand1 = pila.top(); pila.pop();
            switch (token[0]) {
                case '+':
                    pila.push(operand1 + operand2);
                    break;
                case '-':
                    pila.push(operand1 - operand2);
                    break;
                case '*':
                    pila.push(operand1 * operand2);
                    break;
                case '/':
                    pila.push(operand1 / operand2);
                    break;
                case '%':
                    pila.push(fmod(operand1, operand2));
                    break;
                case '^':
                    pila.push(pow(operand1, operand2));
                    break;
            }
          if(MostrarInterfaz){  imprimirEstadoPila(pila);}
        }
    }
    return pila.top();
}

bool Evaluador::evaluarInfija(const string& expresion) {
    stack<char> pila;
    bool ultimoEsOperador = true;
    bool puntoEnNumero = false;
    bool huboDivision = false;

    for (size_t i = 0; i < expresion.length(); ++i) {
        char c = expresion[i];

        if (isdigit(c)) {
            if (huboDivision) {
                if (c == '0') {
                    size_t j = i + 1;
                    while (j < expresion.length() && (isdigit(expresion[j]) || expresion[j] == '.')) {
                        if (expresion[j] != '0') {
                            break;
                        }
                        j++;
                    }
                    if (j == expresion.length() || !isdigit(expresion[j])) {
                        string mensaje = "Error: Expresion no se puede evaluar por cero.";
                      if(MostrarInterfaz){  cout << mensaje << endl;
                        showAlert(mensaje);}
                        return false;
                    }
                }
                huboDivision = false;
            }
            ultimoEsOperador = false;
            puntoEnNumero = false;
        } else if (c == '.') {
            if (puntoEnNumero) {
                string mensaje = "Error: Numero inválido con multiples puntos.";

                if(MostrarInterfaz){cout << mensaje << endl;showAlert(mensaje);}
                return false;
            }
            if (ultimoEsOperador) {
                string mensaje = "Error: El punto decimal no puede estar despues de un operador o al inicio.";

             if(MostrarInterfaz){  cout << mensaje << endl;showAlert(mensaje);}
                return false;
            }
            puntoEnNumero = true;
        } else if (esOperador(c)) {
            if (ultimoEsOperador) {
                string mensaje = "Error: Dos operadores seguidos o expresion comenzando con operador.";

                if(MostrarInterfaz){cout << mensaje << endl;showAlert(mensaje);}
                return false;
            }
            if (c == '/' || c == '%') {
                huboDivision = true;
            }
            ultimoEsOperador = true;
        } else if (c == '(') {
            pila.push(c);
            ultimoEsOperador = true;
        } else if (c == ')') {
            if (pila.empty() || pila.top() != '(') {
                string mensaje = "Error: Parentesis desbalanceados.";

                if(MostrarInterfaz){cout << mensaje << endl;showAlert(mensaje);}
                return false;
            }
            pila.pop();
            ultimoEsOperador = false;
        } else if (!isspace(c)) {
            string mensaje = "Error: Caracter inválido en la expresion.";

            if(MostrarInterfaz){cout << mensaje << endl;showAlert(mensaje);}
            return false;
        }
    }

    if (ultimoEsOperador) {
        string mensaje = "Error: La expresion no puede terminar con un operador.";

        if(MostrarInterfaz){ cout << mensaje << endl;showAlert(mensaje);}
        return false;
    }

    if (!pila.empty()) {
        string mensaje = "Error: Parentesis desbalanceados.";

        if(MostrarInterfaz){ cout << mensaje << endl;showAlert(mensaje);}
        return false;
    }

    return true;
}
