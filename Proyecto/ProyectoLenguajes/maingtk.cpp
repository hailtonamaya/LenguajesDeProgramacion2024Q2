#include <stdlib.h>
#include <gtk/gtk.h>
#include "Evaluador.h"
#include "Constantes.h"
#include <iostream>
#include <string>

#include <fstream>
#include <sstream>

#include "Variables.h"

using namespace std;



GtkWidget *text_view;
GtkWidget *input_entry;
GtkWidget *button;

GtkWidget *variables_input_entry;
GtkWidget *variables_button;



GtkWidget *constant_label;
GtkWidget *history_label;
GtkWidget *variables_label;


GtkWidget *results_label;

GtkWidget *scrolled_window;
GtkWidget *variables_scrolled_window;
GtkWidget *constant_scrolled_window;
GtkWidget *results_scrolled_window;


GtkWidget *hbox;

GtkWidget *panel1, *panel2, *panel3;



GtkWidget *vbox;
GtkWidget *subpanel1, *subpanel2;
GtkWidget *variables_subpanel1, *variables_subpanel2;



GtkWidget *window;

Evaluador Eval(window);
Constantes constas;
Variables vars(constas,window);

string read_file_content(const string& file_path) {
    ifstream file(file_path);
    stringstream buffer;

    if (file) {
        buffer << file.rdbuf();
        return buffer.str();
    } else {
        cerr << "Error al abrir el archivo: " << file_path << endl;
        return "Error: No se pudo leer el archivo.";
    }
}

void show_alert(GtkWindow *parent, const gchar *message) {
    GtkWidget *dialog;
    dialog = gtk_message_dialog_new(parent,
                                    GTK_DIALOG_DESTROY_WITH_PARENT,
                                    GTK_MESSAGE_INFO,
                                    GTK_BUTTONS_OK,
                                    "%s", message);
    gtk_window_set_title(GTK_WINDOW(dialog), "Alerta");
    gtk_dialog_run(GTK_DIALOG(dialog));
    gtk_widget_destroy(dialog);
}


static void on_button_clicked(GtkButton *button, gpointer My_Widgets) {

    GtkWidget **widgets = (GtkWidget **)My_Widgets;
    GtkEntry *entry = GTK_ENTRY(widgets[0]);
    GtkLabel *label = GTK_LABEL(history_label);
     GtkWindow *parent_window = GTK_WINDOW(widgets[1]);

    const gchar *entry_text = gtk_entry_get_text(entry);
    const gchar *current_text = gtk_label_get_text(label);

    string entry_string(entry_text);
    //string expresionInfija = "3 + 4 * 5";
    cout << "Expresion infija: "<< entry_string << endl;

    double resultado = 0;
    //               expresionInfija

    string expresionReemplazadaBase = vars.reemplazarVariables(entry_string);
      string expresionReemplazada = constas.reemplazarConstantes(expresionReemplazadaBase);

        cout << "Expresion infija (VAR): "<< entry_string << endl;

    if(Eval.evaluarInfija(expresionReemplazada)){
     cout << "Expresion infija evaluada correctamente" << endl;
        string expresionPostfija = Eval.infijaAPostfija(expresionReemplazada);
        cout << "Expresion postfija: " << expresionPostfija << endl;
        double resultadoInfija = Eval.evaluarPostfija(expresionPostfija);
             cout << "resultadoInfija: " << resultadoInfija << endl;
        resultado = resultadoInfija;





            cout << "Resultado: " << resultado << endl;

    gchar *new_text = g_strconcat(entry_text, "\n", current_text, NULL);
    gtk_label_set_text(label, new_text); //HISTORIAL


    GtkLabel *labelResults = GTK_LABEL(results_label);
    gchar *tempResult = g_strdup_printf("%f", resultado);
    gtk_label_set_text(labelResults, tempResult); //RESULTADO



    }  else {
    show_alert(parent_window, "Expresion infija no se pudo evaluar");

    }







    //gchar *result_message = g_strdup_printf("Resultado: %f", resultado);
   // show_alert(parent_window, result_message);



}

static void on_variables_button_clicked(GtkButton *button, gpointer My_Widgets) {

    GtkWidget **widgets = (GtkWidget **)My_Widgets;
    GtkEntry *entry = GTK_ENTRY(widgets[0]);
    GtkLabel *label = GTK_LABEL(variables_label);
    GtkWindow *parent_window = GTK_WINDOW(widgets[1]);



    const gchar *entry_text = gtk_entry_get_text(entry);
    string entry_string(entry_text);

    vars.insertarVariable(entry_string);
    vars.imprimirVariables();


    gtk_label_set_text(label, vars.obtenerCadenaDeVariables());



}



int main(int argc, char *argv[]) {

constas.imprimirConstantes();


string file_content = read_file_content("constantes");
const gchar* label_text = file_content.c_str();



    GtkWidget *container;
    GtkWidget *grid;


    gtk_init(&argc, &argv);

    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Evaluador de Expresiones");
    gtk_window_set_default_size(GTK_WINDOW(window), 1000, 500);
    g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);






    hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_container_add(GTK_CONTAINER(window), hbox);


    panel1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    panel2 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    panel3 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);


    GdkRGBA red = {1, 0, 0, 1};
    GdkRGBA green = {0, 1, 0, 1};
    GdkRGBA blue = {0, 0, 1, 0.2};
    GdkRGBA yellow = {1, 1, 0, 0.4};
    GdkRGBA purple = {0.5, 0, 0.5, 0.2};

    gtk_widget_override_background_color(panel1, GTK_STATE_FLAG_NORMAL, &blue);
    gtk_widget_override_background_color(panel2, GTK_STATE_FLAG_NORMAL, NULL );
    gtk_widget_override_background_color(panel3, GTK_STATE_FLAG_NORMAL, &yellow);


    gtk_box_pack_start(GTK_BOX(hbox), panel1, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), panel2, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), panel3, TRUE, TRUE, 0);



    subpanel1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    subpanel2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);



    variables_subpanel1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    variables_subpanel2= gtk_box_new( GTK_ORIENTATION_VERTICAL, 0);




    gtk_widget_override_background_color(subpanel1, GTK_STATE_FLAG_NORMAL, NULL);
    gtk_widget_override_background_color(subpanel2, GTK_STATE_FLAG_NORMAL, &purple);


    gtk_box_pack_start(GTK_BOX(panel2), subpanel1, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(panel2), subpanel2, TRUE, TRUE, 0);



    gtk_box_pack_start(GTK_BOX(panel3), variables_subpanel1, TRUE, TRUE, 10);
    gtk_box_pack_start(GTK_BOX(panel3), variables_subpanel2, TRUE, TRUE, 1);






      //==============================PANEL 1===================================================

    constant_label = gtk_label_new( g_strconcat("Constantes: " , "\n", label_text, NULL));
    gtk_label_set_xalign(GTK_LABEL(constant_label), 0.0);
    gtk_label_set_yalign(GTK_LABEL(constant_label), 0.0);
    gtk_label_set_line_wrap(GTK_LABEL(constant_label), TRUE);
    gtk_label_set_selectable(GTK_LABEL(constant_label), TRUE);


    constant_scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_widget_set_vexpand(constant_scrolled_window, TRUE);
    gtk_widget_set_hexpand(constant_scrolled_window, TRUE);
    gtk_container_add(GTK_CONTAINER(constant_scrolled_window), constant_label);


    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(constant_scrolled_window), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);



    gtk_box_pack_start(GTK_BOX(panel1), constant_scrolled_window, TRUE, TRUE, 0);

    //==================================================================================

    //==================================PANEL 2================================================
    input_entry = gtk_entry_new();
    gtk_entry_set_placeholder_text(GTK_ENTRY(input_entry), "Escribe la expresion aquí...");
    gtk_box_pack_start(GTK_BOX(subpanel1), input_entry, TRUE, TRUE, 0);

    button = gtk_button_new_with_label("Enviar");
    gtk_box_pack_start(GTK_BOX(subpanel1), button, TRUE, TRUE, 0);
    GtkWidget *widgets[2] = { input_entry , window};
    g_signal_connect(button, "clicked", G_CALLBACK(on_button_clicked), widgets);


    results_label = gtk_label_new( g_strconcat(" " , "\n",  NULL));
    gtk_label_set_xalign(GTK_LABEL(results_label), 0.0);
    gtk_label_set_yalign(GTK_LABEL(results_label), 0.0);
    gtk_label_set_line_wrap(GTK_LABEL(results_label), TRUE);
    gtk_label_set_selectable(GTK_LABEL(results_label), TRUE);

    results_scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_widget_set_vexpand(results_scrolled_window, TRUE);
    gtk_widget_set_hexpand(results_scrolled_window, TRUE);
    gtk_container_add(GTK_CONTAINER(results_scrolled_window), results_label);

    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(results_scrolled_window), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);


    gtk_box_pack_start(GTK_BOX(subpanel1), results_scrolled_window, TRUE, TRUE, 0);



    history_label = gtk_label_new("Historial: " );
    gtk_label_set_xalign(GTK_LABEL(history_label), 0.0);
    gtk_label_set_yalign(GTK_LABEL(history_label), 0.0);
    gtk_label_set_line_wrap(GTK_LABEL(history_label), TRUE);
    gtk_label_set_selectable(GTK_LABEL(history_label), TRUE);


    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_widget_set_vexpand(scrolled_window, TRUE);
    gtk_widget_set_hexpand(scrolled_window, TRUE);
    gtk_container_add(GTK_CONTAINER(scrolled_window), history_label);


    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

    gtk_box_pack_start(GTK_BOX(subpanel2), scrolled_window, TRUE, TRUE, 0);

    //==================================================================================


        //==============================PANEL 3===================================================


          //SUBPANEL 1

    variables_label = gtk_label_new("Variables: " );
    gtk_label_set_xalign(GTK_LABEL(variables_label), 0.0);
    gtk_label_set_yalign(GTK_LABEL(variables_label), 0.0);
    gtk_label_set_line_wrap(GTK_LABEL(variables_label), TRUE);
    gtk_label_set_selectable(GTK_LABEL(variables_label), TRUE);


    variables_scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_widget_set_vexpand(variables_scrolled_window, TRUE);
    gtk_widget_set_hexpand(variables_scrolled_window, TRUE);
    gtk_container_add(GTK_CONTAINER(variables_scrolled_window), variables_label);


    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(variables_scrolled_window), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);



    gtk_box_pack_start(GTK_BOX(variables_subpanel1), variables_scrolled_window, TRUE, TRUE, 0);



    //SUBPANEL 2

        variables_input_entry= gtk_entry_new();

      gtk_entry_set_placeholder_text(GTK_ENTRY(variables_input_entry), "Escribe la variable aquí...");
      gtk_box_pack_start(GTK_BOX(variables_subpanel2), variables_input_entry, TRUE, TRUE, 0);

    variables_button = gtk_button_new_with_label("Agregar Variable");
    gtk_box_pack_start(GTK_BOX(variables_subpanel2), variables_button, TRUE, TRUE, 0);



    GtkWidget *widgets2[2] = { variables_input_entry, window};
    g_signal_connect(variables_button, "clicked", G_CALLBACK(on_variables_button_clicked), widgets2);

    //==================================================================================











    gtk_widget_show_all(window);



    gtk_main();

    return EXIT_SUCCESS;
}
//*/
