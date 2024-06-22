#include <stdlib.h>
#include <gtk/gtk.h>


GtkWidget *text_view;
GtkWidget *input_entry;


//SOLO TEXTO
GtkWidget *constant_label;
GtkWidget *history_label;
GtkWidget *variables_label;



//MUCHO TEXTO
GtkWidget *scrolled_window;
GtkWidget *variables_scrolled_window;
GtkWidget *constant_scrolled_window;

GtkWidget *hbox;

GtkWidget *panel1, *panel2, *panel3;

 GtkWidget *vbox;
    GtkWidget *subpanel1, *subpanel2;


void show_text(const gchar *text) {
    GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(text_view));
    gtk_text_buffer_set_text(buffer, text, -1);
}


void handle_expression_button(GtkWidget *widget, gpointer data) {
    const gchar *expression = gtk_entry_get_text(GTK_ENTRY(input_entry));

    const gchar *current_text = gtk_label_get_text(GTK_LABEL(history_label));
    gchar *new_text = g_strdup_printf("%s\n%s", current_text, expression);
    gtk_label_set_text(GTK_LABEL(history_label), new_text);
    g_free(new_text);
}

int main(int argc, char *argv[]) {
    GtkWidget *window;
    GtkWidget *container;
    GtkWidget *grid;
    GtkWidget *button;

    gtk_init(&argc, &argv);

    window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), "Evaluador de Expresiones");
    gtk_window_set_default_size(GTK_WINDOW(window), 1000, 500);
    g_signal_connect(window, "destroy", G_CALLBACK(gtk_main_quit), NULL);






    hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_container_add(GTK_CONTAINER(window), hbox);

    // Crear los tres paneles
    panel1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    panel2 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    panel3 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);

    // Asignar colores diferentes a cada panel
    GdkRGBA red = {1, 0, 0, 1};
    GdkRGBA green = {0, 1, 0, 1};
    GdkRGBA blue = {0, 0, 1, 1};
    GdkRGBA yellow = {1, 1, 0, 1}; // Amarillo
    GdkRGBA purple = {0.5, 0, 0.5, 0.2}; // Púrpura

    gtk_widget_override_background_color(panel1, GTK_STATE_FLAG_NORMAL, &red);
    gtk_widget_override_background_color(panel2, GTK_STATE_FLAG_NORMAL, &green);
    gtk_widget_override_background_color(panel3, GTK_STATE_FLAG_NORMAL, &yellow);

    // Añadir los paneles al contenedor horizontal
    gtk_box_pack_start(GTK_BOX(hbox), panel1, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), panel2, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), panel3, TRUE, TRUE, 0);


// Crear subpaneles para panel2
    subpanel1 = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
    subpanel2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);

    // Asignar colores diferentes a los subpaneles
    gtk_widget_override_background_color(subpanel1, GTK_STATE_FLAG_NORMAL, &yellow);
    gtk_widget_override_background_color(subpanel2, GTK_STATE_FLAG_NORMAL, &purple);


    gtk_box_pack_start(GTK_BOX(panel2), subpanel1, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(panel2), subpanel2, TRUE, TRUE, 0);



     // Crear un Label para mostrar el historial de expresiones
   // history_label = gtk_label_new("Historial: ");
   // gtk_grid_attach(GTK_GRID(grid), history_label, 0, 2, 1, 1);

    //==================================================================================

    history_label = gtk_label_new("Historial: " );
    gtk_label_set_xalign(GTK_LABEL(history_label), 0.0); // Alinear texto a la izquierda
    gtk_label_set_yalign(GTK_LABEL(history_label), 0.0); // Alinear texto en la parte superior
    gtk_label_set_line_wrap(GTK_LABEL(history_label), TRUE); // Habilitar el salto de línea
    gtk_label_set_selectable(GTK_LABEL(history_label), TRUE);

    // Crear un GtkScrolledWindow
    scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_widget_set_vexpand(scrolled_window, TRUE); // Permitir que el scrolled window se expanda verticalmente
    gtk_widget_set_hexpand(scrolled_window, TRUE); // Permitir que el scrolled window se expanda horizontalmente
    gtk_container_add(GTK_CONTAINER(scrolled_window), history_label);

    // Configurar las políticas de desplazamiento
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled_window), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);


    // Añadir el GtkScrolledWindow al tercer panel
    gtk_box_pack_start(GTK_BOX(subpanel2), scrolled_window, TRUE, TRUE, 0);

    //==================================================================================








        //==============================PANEL 3===================================================

    variables_label = gtk_label_new("Variables: " );
    gtk_label_set_xalign(GTK_LABEL(variables_label), 0.0); // Alinear texto a la izquierda
    gtk_label_set_yalign(GTK_LABEL(variables_label), 0.0); // Alinear texto en la parte superior
    gtk_label_set_line_wrap(GTK_LABEL(variables_label), TRUE); // Habilitar el salto de línea
    gtk_label_set_selectable(GTK_LABEL(variables_label), TRUE);

    // Crear un GtkScrolledWindow
    variables_scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_widget_set_vexpand(variables_scrolled_window, TRUE); // Permitir que el scrolled window se expanda verticalmente
    gtk_widget_set_hexpand(variables_scrolled_window, TRUE); // Permitir que el scrolled window se expanda horizontalmente
    gtk_container_add(GTK_CONTAINER(variables_scrolled_window), variables_label);

    // Configurar las políticas de desplazamiento
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(variables_scrolled_window), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);


    // Añadir el GtkScrolledWindow al tercer panel
    gtk_box_pack_start(GTK_BOX(panel3), variables_scrolled_window, TRUE, TRUE, 0);

    //==================================================================================



      //==============================PANEL 1===================================================

    constant_label = gtk_label_new("Constantes: " );
    gtk_label_set_xalign(GTK_LABEL(constant_label), 0.0); // Alinear texto a la izquierda
    gtk_label_set_yalign(GTK_LABEL(constant_label), 0.0); // Alinear texto en la parte superior
    gtk_label_set_line_wrap(GTK_LABEL(constant_label), TRUE); // Habilitar el salto de línea
    gtk_label_set_selectable(GTK_LABEL(constant_label), TRUE);

    // Crear un GtkScrolledWindow
    constant_scrolled_window = gtk_scrolled_window_new(NULL, NULL);
    gtk_widget_set_vexpand(constant_scrolled_window, TRUE); // Permitir que el scrolled window se expanda verticalmente
    gtk_widget_set_hexpand(constant_scrolled_window, TRUE); // Permitir que el scrolled window se expanda horizontalmente
    gtk_container_add(GTK_CONTAINER(constant_scrolled_window), constant_label);

    // Configurar las políticas de desplazamiento
    gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(constant_scrolled_window), GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);


    // Añadir el GtkScrolledWindow al tercer panel
    gtk_box_pack_start(GTK_BOX(panel1), constant_scrolled_window, TRUE, TRUE, 0);

    //==================================================================================



input_entry = gtk_entry_new();
   // gtk_grid_attach(GTK_GRID(grid), input_entry, 0, 1, 1, 1);


  gtk_box_pack_start(GTK_BOX(subpanel1), input_entry, TRUE, TRUE, 0);


   button = gtk_button_new_with_label("Click Me");
    gtk_box_pack_start(GTK_BOX(subpanel1), button, TRUE, TRUE, 0);

    gtk_widget_show_all(window);
    //========================================================================

    // Crear un contenedor para organizar los widgets
  //  container = gtk_box_new(GTK_ORIENTATION_VERTICAL, 0);
   // gtk_container_add(GTK_CONTAINER(panel1), container);

    // Crear una rejilla para dividir la ventana en secciones
   // grid = gtk_grid_new();
    //gtk_box_pack_start(GTK_BOX(container), grid, TRUE, TRUE, 0);

    // Crear un TextView para mostrar el texto del archivo
  //  text_view = gtk_text_view_new();
   // gtk_text_view_set_editable(GTK_TEXT_VIEW(text_view), FALSE);
    //gtk_grid_attach(GTK_GRID(grid), text_view, 0, 0, 1, 1);

    // Crear un Entry para ingresar expresiones
    //input_entry = gtk_entry_new();
   // gtk_grid_attach(GTK_GRID(grid), input_entry, 0, 1, 1, 1);

    // Crear un Label para mostrar el historial de expresiones
   // history_label = gtk_label_new("Historial: ");
   // gtk_grid_attach(GTK_GRID(grid), history_label, 0, 2, 1, 1);

    // Crear un botón para agregar la expresión al historial
   // button = gtk_button_new_with_label("Agregar expresión");
  //  g_signal_connect(button, "clicked", G_CALLBACK(handle_expression_button), NULL);
  //  gtk_grid_attach(GTK_GRID(grid), button, 1, 1, 1, 1);

    // Crear un Label para mostrar las variables generadas
    //variables_label = gtk_label_new("Variables:");
   // gtk_grid_attach(GTK_GRID(grid), variables_label, 0, 3, 1, 1);

    //    GtkCssProvider *provider = gtk_css_provider_new();
    //gtk_css_provider_load_from_data(provider,
    //    "#text_view { background-color: red; }"
    //    "#input_entry { background-color: green; }"
    //    "#history_label { background-color: blue; }"
     //   "#variables_label { background-color: yellow; }",
     //   -1, NULL);









          //========================================================================



    // Mostrar todos los widgets
    gtk_widget_show_all(window);

    gtk_main();

    return EXIT_SUCCESS;
}



/*
#include <gtk/gtk.h>

static void activate(GtkApplication *app, gpointer user_data) {
    GtkWidget *window;
    GtkWidget *hbox;
    GtkWidget *panel1, *panel2, *panel3;

    window = gtk_application_window_new(app);
    gtk_window_set_title(GTK_WINDOW(window), "Three Panels Example");
    gtk_window_set_default_size(GTK_WINDOW(window), 600, 400);

    hbox = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    gtk_container_add(GTK_CONTAINER(window), hbox);

    // Crear los tres paneles
    panel1 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    panel2 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);
    panel3 = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, 0);

    // Asignar colores diferentes a cada panel
    GdkRGBA red = {1, 0, 0, 1};
    GdkRGBA green = {0, 1, 0, 1};
    GdkRGBA blue = {0, 0, 1, 1};

    gtk_widget_override_background_color(panel1, GTK_STATE_FLAG_NORMAL, &red);
    gtk_widget_override_background_color(panel2, GTK_STATE_FLAG_NORMAL, &green);
    gtk_widget_override_background_color(panel3, GTK_STATE_FLAG_NORMAL, &blue);

    // Añadir los paneles al contenedor horizontal
    gtk_box_pack_start(GTK_BOX(hbox), panel1, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), panel2, TRUE, TRUE, 0);
    gtk_box_pack_start(GTK_BOX(hbox), panel3, TRUE, TRUE, 0);

    gtk_widget_show_all(window);
}

int main(int argc, char **argv) {
    GtkApplication *app;
    int status;

    app = gtk_application_new("com.example.threepanels", G_APPLICATION_FLAGS_NONE);
    g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);
    status = g_application_run(G_APPLICATION(app), argc, argv);
    g_object_unref(app);

    return status;
}

*/
