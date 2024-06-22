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

//int main(int argc, char **argv) {
 //   GtkApplication *app;
 //   int status;

  //  app = gtk_application_new("com.example.threepanels", G_APPLICATION_FLAGS_NONE);
  //  g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);
   // status = g_application_run(G_APPLICATION(app), argc, argv);
 //   g_object_unref(app);
//
  //  return status;
//}
