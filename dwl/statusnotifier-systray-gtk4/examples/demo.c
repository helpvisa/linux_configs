#include <snsystray.h>

#include <gdk/gdk.h>
#include <gio/gio.h>
#include <glib-object.h>
#include <glib.h>
#include <gtk/gtk.h>

#include <stdlib.h>

static const int iconsize = 22;
static const int margins = 10;
static const int spacing = 4;
static const int win_width = 240;
static const int win_height = 120;

static void
activate(GtkApplication *app, void *data)
{
	GdkDisplay     *display = gdk_display_get_default();
	GListModel     *mons = gdk_display_get_monitors(display);
	GtkCssProvider *css = gtk_css_provider_new();

	// Set custom background color: many systray icons are not
	// visible against light background.
	// Can also be set via theme css.
	gtk_css_provider_load_from_string(css,
	                                  "systray{background-color:#222222;"
	                                  "}");
	gtk_style_context_add_provider_for_display(display,
	                                           GTK_STYLE_PROVIDER(css),
	                                           GTK_STYLE_PROVIDER_PRIORITY_USER);
	g_object_unref(css);

	// Create tray for each monitor
	for (unsigned int i = 0; i < g_list_model_get_n_items(mons); i++) {
		GtkWidget *widg_window;
		GtkWindow *window;
		GtkWidget *widg_tray;
		SnSystray *tray;

		widg_window = gtk_window_new();
		window = GTK_WINDOW(widg_window);
		gtk_window_set_decorated(window, FALSE);
		gtk_window_set_default_size(window, win_width, win_height);

		tray = sn_systray_new(iconsize, spacing, margins, NULL);
		widg_tray = GTK_WIDGET(tray);

		gtk_widget_set_halign(widg_tray, GTK_ALIGN_CENTER);
		gtk_widget_set_valign(widg_tray, GTK_ALIGN_CENTER);

		gtk_window_set_child(window, widg_tray);
		gtk_window_set_application(window, app);

		gtk_window_present(window);
	}
}

int
main(int argc, char *argv[])
{
	int             status;
	GtkApplication *app = gtk_application_new("org.example.demo",
	                                          G_APPLICATION_NON_UNIQUE);

	g_signal_connect(app, "activate", G_CALLBACK(activate), NULL);

	status = g_application_run(G_APPLICATION(app), 0, NULL);

	g_object_unref(app);
	return status;
}
