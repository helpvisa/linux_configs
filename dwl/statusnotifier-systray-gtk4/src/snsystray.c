/*
 *   SPDX-FileCopyrightText: 2024 Janne Veteläinen <janne.vetelainen@elisanet.fi>
 *
 *   SPDX-License-Identifier: GPL-3.0-only
 */

/**
 * SnSystray:
 *
 * `SnSystray` is a system tray widget, implementing the server side of
 * KDE's KStatusNotifierItem specification. It registers the name
 * `org.kde.StatusNotifierWatcher` on the DBus session bus, and listens for
 * applications wanting to register system tray items.
 *
 * `SnSystray` wraps the mentioned DBus data to [class@Gtk.Image] widgets,
 * which have a [class@Gtk.PopoverMenu] attached to them, and arranges them
 * into a [class@Gtk.Box].
 */

#include "snsystray.h"

#include "snitem.h"
#include "snwatcher.h"

#include <glib-object.h>
#include <glib.h>
#include <gtk/gtk.h>

#include <limits.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

struct _SnSystray {
	GtkWidget parent_instance;

	char         *hostid;
	GtkWidget    *box;
	SnWatcher    *watcher;
	unsigned long reg_sub_id;
	unsigned long unreg_sub_id;
	int           spacing;
	int           iconsize;
	int           box_margins;
	int           nchildren;
	int           curwidth;
	int           curheight;
	gboolean      exiting;
};

G_DEFINE_FINAL_TYPE(SnSystray, sn_systray, GTK_TYPE_WIDGET)

enum
{
	PROP_ICONSIZE = 1,
	PROP_MARGINS,
	PROP_SPACING,
	PROP_HOSTID,
	PROP_CURWIDTH,
	PROP_CURHEIGHT,
	N_PROPERTIES
};

static GParamSpec *obj_properties[N_PROPERTIES] = {NULL};

static void sn_systray_constructed (GObject *obj);
static void sn_systray_dispose (GObject *obj);
static void sn_systray_finalize (GObject *obj);
static void sn_systray_set_height_prop (SnSystray *self);
static void sn_systray_set_width_prop (SnSystray *self);

static GtkWidget *
find_child(GtkWidget *box, const char *busname)
{
	GtkWidget *result = NULL;

	GtkWidget *iter;
	SnItem    *item;
	char      *item_busname;

	for (iter = gtk_widget_get_first_child(box); iter != NULL;
	     iter = gtk_widget_get_next_sibling(iter))
	{
		item = SN_ITEM(iter);
		item_busname = sn_item_get_busname(item);
		if (item_busname && strcmp(item_busname, busname) == 0) {
			result = iter;
		}
		g_free(item_busname);
	}

	return result;
}

static void
register_item(SnWatcher  *watcher,
              const char *busname,
              const char *busobj,
              SnSystray  *self)
{
	GtkBox *box = GTK_BOX(self->box);
	SnItem *snitem;

	if (find_child(self->box, busname) != NULL) {
		g_warning("Already tracked");
		return;
	}

	snitem = sn_item_new(busname, busobj, self->iconsize);

	gtk_box_append(box, GTK_WIDGET(snitem));
	self->nchildren = self->nchildren + 1;
	sn_systray_set_width_prop(self);
	sn_systray_set_height_prop(self);
}

static void
unregister_item(SnWatcher *watcher, const char *busname, SnSystray *self)
{
	GtkWidget *child;
	GtkBox    *box = GTK_BOX(self->box);

	child = find_child(self->box, busname);

	if (child != NULL) {
		gtk_box_remove(box, child);
		self->nchildren = self->nchildren - 1;
		sn_systray_set_width_prop(self);
		sn_systray_set_height_prop(self);
	}
}

static void
sn_systray_set_height_prop(SnSystray *self)
{
	self->curheight = self->iconsize + self->box_margins * 2;

	g_object_notify_by_pspec(G_OBJECT(self), obj_properties[PROP_CURHEIGHT]);
}

static void
sn_systray_set_width_prop(SnSystray *self)
{
	if (self->nchildren <= 1) {
		// Width of 1 icon even when there are none.
		self->curwidth = self->iconsize + 2 * self->box_margins;
	} else {
		self->curwidth =
			// Icons themselves.
			self->nchildren * self->iconsize +

			// Spacing between icons.
			(self->nchildren - 1) * self->spacing +

			// Margins before first icon and after last icon.
			2 * self->box_margins;
	}

	g_object_notify_by_pspec(G_OBJECT(self), obj_properties[PROP_CURWIDTH]);
}

static void
sn_systray_set_hostid(SnSystray *self, const char *hostid)
{
	if (hostid != NULL)
		self->hostid = g_strdup(hostid);
	else
		self->hostid = g_uuid_string_random();
}

static void
sn_systray_get_property(GObject     *object,
                        unsigned int property_id,
                        GValue      *value,
                        GParamSpec  *pspec)
{
	SnSystray *self = SN_SYSTRAY(object);

	switch (property_id) {
	case PROP_CURWIDTH:
		g_value_set_int(value, self->curwidth);
		break;
	case PROP_CURHEIGHT:
		g_value_set_int(value, self->curwidth);
		break;
	case PROP_HOSTID:
		g_value_set_string(value, self->hostid);
		break;
	default:
		g_assert_not_reached();
		break;
	}
}
static void
sn_systray_set_property(GObject      *object,
                        unsigned int  property_id,
                        const GValue *value,
                        GParamSpec   *pspec)
{
	SnSystray *self = SN_SYSTRAY(object);

	switch (property_id) {
	case PROP_SPACING:
		self->spacing = g_value_get_int(value);
		break;
	case PROP_ICONSIZE:
		self->iconsize = g_value_get_int(value);
		break;
	case PROP_MARGINS:
		self->box_margins = g_value_get_int(value);
		break;
	case PROP_HOSTID:
		sn_systray_set_hostid(self, g_value_get_string(value));
		break;
	default:
		g_assert_not_reached();
		break;
	}
}

static void
sn_systray_class_init(SnSystrayClass *klass)
{
	GObjectClass   *object_class = G_OBJECT_CLASS(klass);
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(klass);

	object_class->get_property = sn_systray_get_property;
	object_class->set_property = sn_systray_set_property;
	object_class->constructed = sn_systray_constructed;
	object_class->dispose = sn_systray_dispose;
	object_class->finalize = sn_systray_finalize;

	/**
	 * SnSystray:iconsize:
	 *
	 * Size of the icon for new tray items.
	 */
	obj_properties[PROP_ICONSIZE] =
		g_param_spec_int("iconsize",
	                         NULL,
	                         NULL,
	                         INT_MIN,
	                         INT_MAX,
	                         22,
	                         G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE |
	                                 G_PARAM_STATIC_STRINGS);

	/**
	 * SnSystray:spacing:
	 *
	 * Spacing between tray items.
	 */
	obj_properties[PROP_SPACING] =
		g_param_spec_int("spacing",
	                         NULL,
	                         NULL,
	                         INT_MIN,
	                         INT_MAX,
	                         4,
	                         G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE |
	                                 G_PARAM_STATIC_STRINGS);

	/**
	 * SnSystray:box-margins:
	 *
	 * Inner margins of the GtkBox.
	 */
	obj_properties[PROP_MARGINS] =
		g_param_spec_int("box-margins",
	                         NULL,
	                         NULL,
	                         INT_MIN,
	                         INT_MAX,
	                         4,
	                         G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE |
	                                 G_PARAM_STATIC_STRINGS);

	/**
	 * SnSystray:host-id:
	 *
	 * Unique identifier string.
	 *
	 * If this SnSystray is part of a desktop shell panel, which of there
	 * are one for each monitor, it may be useful to set this to the
	 * monitor's connector name.
	 */
	obj_properties[PROP_HOSTID] =
		g_param_spec_string("host-id",
	                            NULL,
	                            NULL,
	                            NULL,
	                            G_PARAM_CONSTRUCT_ONLY | G_PARAM_READWRITE |
	                                    G_PARAM_STATIC_STRINGS);

	/**
	 * SnSystray:curwidth:
	 *
	 * Current width of the widget.
	 */
	obj_properties[PROP_CURWIDTH] =
		g_param_spec_int("curwidth",
	                         NULL,
	                         NULL,
	                         INT_MIN,
	                         INT_MAX,
	                         0,
	                         G_PARAM_EXPLICIT_NOTIFY | G_PARAM_READABLE |
	                                 G_PARAM_STATIC_STRINGS);

	/**
	 * SnSystray:curheight:
	 *
	 * Current height of the widget.
	 */
	obj_properties[PROP_CURHEIGHT] =
		g_param_spec_int("curheight",
	                         NULL,
	                         NULL,
	                         INT_MIN,
	                         INT_MAX,
	                         0,
	                         G_PARAM_EXPLICIT_NOTIFY | G_PARAM_READABLE |
	                                 G_PARAM_STATIC_STRINGS);

	g_object_class_install_properties(object_class,
	                                  N_PROPERTIES,
	                                  obj_properties);

	gtk_widget_class_set_css_name(widget_class, "systray");

	gtk_widget_class_set_layout_manager_type(widget_class,
	                                         GTK_TYPE_BIN_LAYOUT);
}

static void
sn_systray_init(SnSystray *self)
{
	self->watcher = sn_watcher_get_default();
}

static void
sn_systray_constructed(GObject *obj)
{
	SnSystray *self = SN_SYSTRAY(obj);
	GtkWidget *widget = GTK_WIDGET(obj);

	GtkBox      *box;
	TrackedItem *ti;
	GArray      *snitems;

	g_debug("Created SnSystray %s", self->hostid);

	self->box = gtk_box_new(GTK_ORIENTATION_HORIZONTAL, self->spacing);
	box = GTK_BOX(self->box);

	sn_systray_set_width_prop(self);
	sn_systray_set_height_prop(self);
	gtk_widget_set_size_request(widget, self->curwidth, self->curheight);

	gtk_widget_set_margin_start(self->box, self->box_margins);
	gtk_widget_set_margin_end(self->box, self->box_margins);
	gtk_widget_set_margin_top(self->box, self->box_margins);
	gtk_widget_set_margin_bottom(self->box, self->box_margins);
	gtk_box_set_homogeneous(box, true);

	gtk_widget_set_parent(self->box, widget);

	self->reg_sub_id = g_signal_connect(self->watcher,
	                                    "trayitem-registered",
	                                    G_CALLBACK(register_item),
	                                    self);

	self->unreg_sub_id = g_signal_connect(self->watcher,
	                                      "trayitem-unregistered",
	                                      G_CALLBACK(unregister_item),
	                                      self);

	// Register items that existed before we connected
	// to "trayitem-registered"
	snitems = sn_watcher_get_tracked_items(self->watcher);
	for (unsigned int i = 0; i < snitems->len; i++) {
		ti = &g_array_index(snitems, TrackedItem, i);
		register_item(self->watcher, ti->busname, ti->busobj, self);
	}

	g_array_unref(snitems);

	G_OBJECT_CLASS(sn_systray_parent_class)->constructed(obj);
}

static void
sn_systray_dispose(GObject *obj)
{
	SnSystray *self = SN_SYSTRAY(obj);

	g_debug("Disposing SnSystray %s", self->hostid);

	if (self->reg_sub_id > 0) {
		g_signal_handler_disconnect(self->watcher, self->reg_sub_id);
		self->reg_sub_id = 0;
	}

	if (self->unreg_sub_id > 0) {
		g_signal_handler_disconnect(self->watcher, self->unreg_sub_id);
		self->reg_sub_id = 0;
	}

	if (self->watcher != NULL) {
		g_object_unref(self->watcher);
		self->watcher = NULL;
	}

	if (self->box != NULL) {
		gtk_widget_unparent(self->box);
		self->box = NULL;
	}

	G_OBJECT_CLASS(sn_systray_parent_class)->dispose(obj);
}

static void
sn_systray_finalize(GObject *obj)
{
	SnSystray *self = SN_SYSTRAY(obj);

	g_free(self->hostid);

	G_OBJECT_CLASS(sn_systray_parent_class)->finalize(obj);
}

/**
 * sn_systray_get_height:
 * @systray: A `SnSystray`
 *
 * Gets the current height of @systray.
 *
 * Returns: The current height.
 */
int
sn_systray_get_height(SnSystray *systray)
{
	g_return_val_if_fail(SN_IS_SYSTRAY(systray), -1);

	return systray->curheight;
}

/**
 * sn_systray_get_hostid:
 * @systray: A `SnSystray`
 *
 * Gets the unique identifier of @systray.
 *
 * Returns: (transfer full): The unique identifier string.
 */
char *
sn_systray_get_hostid(SnSystray *systray)
{
	g_return_val_if_fail(SN_IS_SYSTRAY(systray), NULL);

	return g_strdup(systray->hostid);
}

/**
 * sn_systray_get_width:
 * @systray: a `SnSystray`
 *
 * Gets the current width of @systray.
 *
 * Returns: The current width.
 */
int
sn_systray_get_width(SnSystray *systray)
{
	g_return_val_if_fail(SN_IS_SYSTRAY(systray), -1);

	return systray->curwidth;
}

/**
 * sn_systray_new:
 * @iconsize: Icon size in pixels, used for new tray items.
 * @margins: Inner margins of the [class@Gtk.Box] in pixels.
 * @spacing: Spacing between icons in pixels.
 * @host_id: (nullable): Unique identifier. If not set by the caller, it will
 * default to a randomly generated string.
 *
 * Creates a new `SnSystray`.
 *
 * Returns: (transfer full): the new 'SnSystray'
 */
SnSystray *
sn_systray_new(int iconsize, int margins, int spacing, const char *host_id)
{
	return g_object_new(SN_TYPE_SYSTRAY,
	                    "iconsize",
	                    iconsize,
	                    "box-margins",
	                    margins,
	                    "spacing",
	                    spacing,
	                    "host-id",
	                    host_id,
	                    NULL);
}
