/*
 *   SPDX-FileCopyrightText: 2024 Janne Veteläinen <janne.vetelainen@elisanet.fi>
 *
 *   SPDX-License-Identifier: GPL-3.0-only
 */

#include "snitem.h"

#include "sndbusmenu.h"

#include <gdk-pixbuf/gdk-pixbuf.h>
#include <gdk/gdk.h>
#include <gio/gio.h>
#include <glib-object.h>
#include <glib.h>
#include <gtk/gtk.h>

#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

struct _SnItem {
	GtkWidget parent_instance;

	char          iconpath[PATH_MAX];
	char          iconname[NAME_MAX + 1];
	GArray       *cachedicons;
	GDBusProxy   *proxy;
	GMenu        *init_menu;
	GVariant     *iconpixmap;
	GtkGesture   *lclick;
	GtkGesture   *rclick;
	GtkWidget    *image;
	GtkWidget    *popovermenu;
	SnDbusmenu   *dbusmenu;
	char         *busname;
	char         *busobj;
	unsigned long lclick_id;
	unsigned long popup_id;
	unsigned long proxy_id;
	unsigned long rclick_id;
	int           icon_source;
	int           iconsize;
	int           status;
	gboolean      in_destruction;
	gboolean      menu_visible;
	gboolean      update_pending;
};

G_DEFINE_FINAL_TYPE(SnItem, sn_item, GTK_TYPE_WIDGET)

enum
{
	PROP_BUSNAME = 1,
	PROP_BUSOBJ,
	PROP_ICONSIZE,
	PROP_DBUSMENU,
	PROP_MENUVISIBLE,
	N_PROPERTIES
};

enum
{
	RIGHTCLICK,
	LAST_SIGNAL
};

enum icon_sources
{
	ICON_SOURCE_UNKNOWN,
	ICON_SOURCE_NAME,
	ICON_SOURCE_PATH,
	ICON_SOURCE_PIXMAP,
};

static GParamSpec  *obj_properties[N_PROPERTIES] = {NULL};
static unsigned int signals[LAST_SIGNAL];
static const int    update_ival = 200;

typedef struct {
	char          iconpath[PATH_MAX];
	char          iconname[NAME_MAX + 1];
	GVariant     *iconpixmap;
	GdkPaintable *icondata;
} CachedIcon;

static void sn_item_constructed (GObject *obj);
static void sn_item_dispose (GObject *obj);
static void sn_item_finalize (GObject *obj);

static void sn_item_size_allocate (GtkWidget *widget,
                                   int        width,
                                   int        height,
                                   int        baseline);

static void sn_item_measure (GtkWidget     *widget,
                             GtkOrientation orientation,
                             int            for_size,
                             int           *minimum,
                             int           *natural,
                             int           *minimum_baseline,
                             int           *natural_baseline);

static void request_newicon_pixmap (SnItem *self);

static void
destroy_pixbuf(unsigned char *pix, void *data)
{
	g_free(pix);
}

static gboolean
validate_pixdata(GVariant *icondata)
{
	gboolean isvalid = true;

	int32_t   width, height;
	GVariant *bytearr;
	size_t    size;

	g_variant_get_child(icondata, 0, "i", &width);
	g_variant_get_child(icondata, 1, "i", &height);
	bytearr = g_variant_get_child_value(icondata, 2);
	size = g_variant_get_size(bytearr);

	if (width == 0 || height == 0 || size == 0)
		isvalid = false;

	g_variant_unref(bytearr);
	return isvalid;
}

static void
argb_to_rgba(int32_t width, int32_t height, unsigned char *icon_data)
{
	// Icon data is ARGB, gdk textures are RGBA. Flip the channels
	// Copied from Waybar
	for (int32_t i = 0; i < 4 * width * height; i += 4) {
		unsigned char alpha = icon_data[i];
		icon_data[i] = icon_data[i + 1];
		icon_data[i + 1] = icon_data[i + 2];
		icon_data[i + 2] = icon_data[i + 3];
		icon_data[i + 3] = alpha;
	}
}

static gboolean
find_cached_icon_path(const CachedIcon *ci, const char *path)
{
	if (strcmp(ci->iconpath, path) == 0)
		return true;
	else
		return false;
}

static gboolean
find_cached_icon_name(const CachedIcon *ci, const char *name)
{
	if (strcmp(ci->iconname, name) == 0)
		return true;
	else
		return false;
}

static gboolean
find_cached_icon_pixmap(const CachedIcon *ci, GVariant *pixmap)
{
	if (ci->iconpixmap == NULL)
		return false;

	return (g_variant_equal(ci->iconpixmap, pixmap));
}

static void
free_cachedicon(void *data)
{
	CachedIcon *ci = (CachedIcon *)data;

	if (ci->iconpixmap != NULL) {
		g_variant_unref(ci->iconpixmap);
	}

	if (ci->icondata != NULL) {
		g_object_unref(ci->icondata);
	}
}

static GVariant *
select_icon_by_size(GVariant *vicondata, int32_t target_icon_size)
{
	// Apps broadcast icons as variant a(iiay)
	// Meaning array of tuples, tuple representing an icon
	// first 2 members ii in each tuple are width and height
	// We iterate the array and pick the icon size closest to
	// the target based on its width and save the index
	GVariant    *child;
	GVariant    *selected;
	GVariantIter iter;
	int          current_index = 0;
	int          selected_index = 0;
	int32_t      curdiff;
	int32_t      diff = INT32_MAX;

	g_variant_iter_init(&iter, vicondata);
	while ((child = g_variant_iter_next_value(&iter))) {
		int32_t curwidth;
		g_variant_get_child(child, 0, "i", &curwidth);
		if (curwidth > target_icon_size)
			curdiff = curwidth - target_icon_size;
		else
			curdiff = target_icon_size - curwidth;

		if (curdiff < diff)
			selected_index = current_index;

		current_index = current_index + 1;
		g_variant_unref(child);
	}

	selected = g_variant_get_child_value(vicondata, selected_index);

	// Discard if the array is empty
	if (!validate_pixdata(selected)) {
		g_variant_unref(selected);
		return NULL;
	}

	return selected;
}

static GdkPaintable *
get_paintable_from_name(const char *iconname, int32_t iconsize)
{
	GdkPaintable     *paintable = NULL;
	GtkIconPaintable *icon;
	GtkIconTheme     *theme =
		gtk_icon_theme_get_for_display(gdk_display_get_default());

	icon = gtk_icon_theme_lookup_icon(theme,
	                                  iconname,
	                                  NULL, // const char **fallbacks
	                                  iconsize,
	                                  1,
	                                  GTK_TEXT_DIR_LTR,
	                                  0); // GtkIconLookupFlags

	paintable = GDK_PAINTABLE(icon);

	return paintable;
}

static GdkPaintable *
get_paintable_from_path(const char *path, int32_t iconsize)
{
	GdkPaintable *paintable;
	GdkPixbuf    *pixbuf;
	GdkTexture   *texture;

	pixbuf = gdk_pixbuf_new_from_file_at_size(path, iconsize, iconsize, NULL);
	texture = gdk_texture_new_for_pixbuf(pixbuf);

	paintable = GDK_PAINTABLE(texture);

	g_object_unref(pixbuf);
	return paintable;
}

static GdkPaintable *
get_paintable_from_data(GVariant *vpixmap, int32_t iconsize)
{
	GVariant      *vicondata;
	GVariantIter   iter;
	GdkPaintable  *paintable;
	GdkPixbuf     *pixbuf;
	GdkTexture    *texture;
	const void    *icon_data_dup;
	int32_t        height;
	int32_t        rowstride;
	int32_t        width;
	size_t         size;
	unsigned char *icon_data;

	g_variant_iter_init(&iter, vpixmap);
	g_variant_iter_next(&iter, "i", &width);
	g_variant_iter_next(&iter, "i", &height);
	vicondata = g_variant_iter_next_value(&iter);
	size = g_variant_get_size(vicondata);
	icon_data_dup = g_variant_get_data(vicondata);

	icon_data = g_memdup2(icon_data_dup, size);
	argb_to_rgba(width, height, icon_data);
	rowstride = (width * 32 + 7) / 8;

	pixbuf = gdk_pixbuf_new_from_data(icon_data,
	                                  GDK_COLORSPACE_RGB,
	                                  true,
	                                  8,
	                                  width,
	                                  height,
	                                  rowstride,
	                                  destroy_pixbuf,
	                                  NULL);
	texture = gdk_texture_new_for_pixbuf(pixbuf);

	paintable = GDK_PAINTABLE(texture);

	g_object_unref(pixbuf);
	g_variant_unref(vicondata);
	return paintable;
}

static void
update_from_name(const char *iconname, size_t len, SnItem *self)
{
	CachedIcon  ci_new = {0};
	CachedIcon *ci;
	gboolean    cache_hit = false;

	// Icon didn't change
	if (strcmp(iconname, self->iconname) == 0)
		return;

	for (unsigned int i = 0; i < self->cachedicons->len; i++) {
		ci = &g_array_index(self->cachedicons, CachedIcon, i);
		cache_hit = find_cached_icon_name(ci, iconname);
		if (cache_hit)
			break;
	}

	if (cache_hit) {
		memcpy(self->iconname, iconname, len + 1);
		gtk_image_set_from_paintable(GTK_IMAGE(self->image),
		                             ci->icondata);
	} else {
		if (len == 0 || len > NAME_MAX)
			return;

		ci_new.icondata = get_paintable_from_name(iconname,
		                                          self->iconsize);
		memcpy(ci_new.iconname, iconname, len + 1);
		g_array_append_val(self->cachedicons, ci_new);

		memcpy(self->iconname, ci_new.iconname, len + 1);
		gtk_image_set_from_paintable(GTK_IMAGE(self->image),
		                             ci_new.icondata);
	}
}

static void
update_from_path(const char *iconpath, size_t len, SnItem *self)
{
	CachedIcon  ci_new = {0};
	CachedIcon *ci;
	gboolean    cache_hit = false;

	// Icon didn't change
	if (strcmp(iconpath, self->iconpath) == 0)
		return;

	for (unsigned int i = 0; i < self->cachedicons->len; i++) {
		ci = &g_array_index(self->cachedicons, CachedIcon, i);
		cache_hit = find_cached_icon_path(ci, iconpath);
		if (cache_hit)
			break;
	}

	if (cache_hit) {
		memcpy(self->iconpath, iconpath, len + 1);
		gtk_image_set_from_paintable(GTK_IMAGE(self->image),
		                             ci->icondata);
	} else {
		if (len + 1 > PATH_MAX)
			return;

		ci_new.icondata = get_paintable_from_path(iconpath,
		                                          self->iconsize);
		memcpy(ci_new.iconpath, iconpath, len + 1);
		g_array_append_val(self->cachedicons, ci_new);

		memcpy(self->iconpath, ci_new.iconpath, len + 1);
		gtk_image_set_from_paintable(GTK_IMAGE(self->image),
		                             ci_new.icondata);
	}
}

static void
new_iconname_or_path_handler(GObject *obj, GAsyncResult *res, void *data)
{
	SnItem     *self = SN_ITEM(data);
	GDBusProxy *proxy = G_DBUS_PROXY(obj);

	GError     *err = NULL;
	GVariant   *vnameorpath;
	const char *nameorpath;
	size_t      len;

	GVariant *retvariant = g_dbus_proxy_call_finish(proxy, res, &err);

	if (err != NULL) {
		switch (err->code) {
		case G_DBUS_ERROR_UNKNOWN_PROPERTY:
			request_newicon_pixmap(g_object_ref(self));
			break;
		case G_DBUS_ERROR_UNKNOWN_OBJECT:
			// Remote object went away
			break;
		case G_DBUS_ERROR_SERVICE_UNKNOWN:
			// Remote service went away
			break;
		default:
			g_warning("%s\n", err->message);
			break;
		}

		g_error_free(err);
		g_object_unref(self);
		return;
	}

	g_variant_get(retvariant, "(v)", &vnameorpath);
	nameorpath = g_variant_get_string(vnameorpath, &len);

	if (len == 0) {
		// New iconname invalid
		self->icon_source = ICON_SOURCE_UNKNOWN;
		request_newicon_pixmap(g_object_ref(self));
	} else if (access(nameorpath, R_OK) == 0) {
		self->icon_source = ICON_SOURCE_PATH;
		update_from_path(nameorpath, len, self);
	} else {
		self->icon_source = ICON_SOURCE_NAME;
		update_from_name(nameorpath, len, self);
	}

	g_variant_unref(vnameorpath);
	g_variant_unref(retvariant);
	g_object_unref(self);
}

static void
new_pixmaps_handler(GObject *obj, GAsyncResult *res, void *data)
{
	SnItem     *self = SN_ITEM(data);
	GDBusProxy *proxy = G_DBUS_PROXY(obj);

	CachedIcon    ci_new = {0};
	CachedIcon   *ci;
	GVariant     *newpixmaps;
	GVariant     *pixmap;
	GdkPaintable *paintable;
	gboolean      cache_hit = false;

	GVariant *retvariant = g_dbus_proxy_call_finish(proxy, res, NULL);

	if (retvariant == NULL) {
		g_object_unref(self);
		return;
	}

	g_variant_get(retvariant, "(v)", &newpixmaps);
	pixmap = select_icon_by_size(newpixmaps, self->iconsize);

	// No valid icon in data
	if (pixmap == NULL) {
		g_debug("no valid icon");
		self->icon_source = ICON_SOURCE_UNKNOWN;
		g_variant_unref(newpixmaps);
		g_variant_unref(retvariant);
		g_object_unref(self);
		return;
	}

	// Icon didn't change
	if (self->iconpixmap != NULL && g_variant_equal(pixmap, self->iconpixmap))
	{
		g_debug("icon didnt change");
		g_variant_unref(pixmap);
		g_variant_unref(newpixmaps);
		g_variant_unref(retvariant);
		g_object_unref(self);
		return;
	}

	for (unsigned int i = 0; i < self->cachedicons->len; i++) {
		ci = &g_array_index(self->cachedicons, CachedIcon, i);
		cache_hit = find_cached_icon_pixmap(ci, pixmap);
		if (cache_hit)
			break;
	}

	if (cache_hit) {
		g_variant_unref(self->iconpixmap);
		self->iconpixmap = g_variant_ref(ci->iconpixmap);
		gtk_image_set_from_paintable(GTK_IMAGE(self->image),
		                             ci->icondata);
	} else {
		paintable = get_paintable_from_data(pixmap, self->iconsize);

		ci_new.iconpixmap = g_variant_ref(pixmap);
		ci_new.icondata = paintable;
		g_array_append_val(self->cachedicons, ci_new);

		if (self->iconpixmap)
			g_variant_unref(self->iconpixmap);
		self->iconpixmap = g_variant_ref(ci_new.iconpixmap);
		gtk_image_set_from_paintable(GTK_IMAGE(self->image),
		                             ci_new.icondata);
		self->icon_source = ICON_SOURCE_PIXMAP;
	}

	g_variant_unref(pixmap);
	g_variant_unref(newpixmaps);
	g_variant_unref(retvariant);
	g_object_unref(self);
}

static void
request_newicon_name_or_path(SnItem *self)
{
	self->update_pending = false;

	g_dbus_proxy_call(self->proxy,
	                  "org.freedesktop.DBus.Properties.Get",
	                  g_variant_new("(ss)",
	                                "org.kde.StatusNotifierItem",
	                                "IconName"),
	                  G_DBUS_CALL_FLAGS_NONE,
	                  -1,
	                  NULL,
	                  new_iconname_or_path_handler,
	                  g_object_ref(self));

	g_object_unref(self);
}

static void
request_newicon_pixmap(SnItem *self)
{
	self->update_pending = false;

	g_dbus_proxy_call(self->proxy,
	                  "org.freedesktop.DBus.Properties.Get",
	                  g_variant_new("(ss)",
	                                "org.kde.StatusNotifierItem",
	                                "IconPixmap"),
	                  G_DBUS_CALL_FLAGS_NONE,
	                  -1,
	                  NULL,
	                  new_pixmaps_handler,
	                  g_object_ref(self));

	g_object_unref(self);
}

static void
proxy_signal_handler(GDBusProxy *proxy,
                     const char *sender,
                     const char *signal,
                     GVariant   *data_v,
                     void       *data)
{
	SnItem *self = SN_ITEM(data);
	void (*fun)(SnItem *item);

	if (self->update_pending)
		return;

	self->update_pending = true;

	if (strcmp(signal, "NewIcon") == 0) {
		switch (self->icon_source) {
		case ICON_SOURCE_NAME:
			fun = request_newicon_name_or_path;
			break;
		case ICON_SOURCE_PATH:
			fun = request_newicon_name_or_path;
			break;
		case ICON_SOURCE_PIXMAP:
			fun = request_newicon_pixmap;
			break;
		default:
			fun = request_newicon_name_or_path;
			break;
		}
		g_timeout_add_once(update_ival,
		                   (GSourceOnceFunc)fun,
		                   g_object_ref(self));
	}
}

static void
popup_popover(SnDbusmenu *dbusmenu, SnItem *self)
{
	if (self->in_destruction)
		return;

	g_object_set(self, "menuvisible", true, NULL);
	gtk_popover_popup(GTK_POPOVER(self->popovermenu));
}

static void
leftclick_handler(GtkGestureClick *click, int n_press, double x, double y, void *data)
{
	SnItem *self = SN_ITEM(data);

	g_dbus_proxy_call(self->proxy,
	                  "Activate",
	                  g_variant_new("(ii)", 0, 0),
	                  G_DBUS_CALL_FLAGS_NONE,
	                  -1,
	                  NULL,
	                  NULL,
	                  NULL);
}

static void
rclick_handler(GtkGestureClick *click, int n_press, double x, double y, void *data)
{
	SnItem *self = SN_ITEM(data);

	if (self->in_destruction)
		return;

	g_signal_emit(self, signals[RIGHTCLICK], 0);
}

static void
connect_to_menu(SnItem *self)
{
	GVariant   *vmenupath;
	const char *menu_buspath = NULL;

	if (self->in_destruction)
		return;

	vmenupath = g_dbus_proxy_get_cached_property(self->proxy, "Menu");

	if (vmenupath != NULL) {
		g_variant_get(vmenupath, "&o", &menu_buspath);
		if (strcmp(menu_buspath, "") != 0) {
			self->dbusmenu = sn_dbusmenu_new(self->busname,
			                                 menu_buspath,
			                                 self);

			self->rclick_id = g_signal_connect(self->rclick,
			                                   "pressed",
			                                   G_CALLBACK(rclick_handler),
			                                   self);

			self->popup_id = g_signal_connect(self->dbusmenu,
			                                  "abouttoshowhandled",
			                                  G_CALLBACK(popup_popover),
			                                  self);
		}
		g_variant_unref(vmenupath);
	}
}

static void
select_icon_source(SnItem *self)
{
	GVariant   *vname;
	GVariant   *vpixmaps;
	const char *nameorpath;
	size_t      len = 0;

	vname = g_dbus_proxy_get_cached_property(self->proxy, "IconName");
	vpixmaps = g_dbus_proxy_get_cached_property(self->proxy, "IconPixmap");

	if (vname != NULL)
		nameorpath = g_variant_get_string(vname, &len);

	if (len != 0 && len + 1 <= PATH_MAX && access(nameorpath, R_OK) == 0) {
		memcpy(self->iconpath, nameorpath, len + 1);
		self->icon_source = ICON_SOURCE_PATH;
	} else if (len != 0 && len <= NAME_MAX) {
		memcpy(self->iconname, nameorpath, len + 1);
		self->icon_source = ICON_SOURCE_NAME;
	} else if (vpixmaps != NULL) {
		self->iconpixmap = select_icon_by_size(vpixmaps, self->iconsize);
		if (self->iconpixmap != NULL)
			self->icon_source = ICON_SOURCE_PIXMAP;
	} else {
		strcpy(self->iconname, "no-icon");
		self->icon_source = ICON_SOURCE_UNKNOWN;
	}

	if (vname != NULL)
		g_variant_unref(vname);
	if (vpixmaps != NULL)
		g_variant_unref(vpixmaps);
}

static void
add_icontheme_path(GDBusProxy *proxy)
{
	GVariant     *viconthemepath;
	GtkIconTheme *theme;
	const char   *iconthemepath;

	viconthemepath = g_dbus_proxy_get_cached_property(proxy,
	                                                  "IconThemePath");
	theme = gtk_icon_theme_get_for_display(gdk_display_get_default());

	if (viconthemepath != NULL) {
		g_variant_get(viconthemepath, "&s", &iconthemepath);
		gtk_icon_theme_add_search_path(theme, iconthemepath);

		g_variant_unref(viconthemepath);
	}
}

static void
proxy_ready_handler(GObject *obj, GAsyncResult *res, void *data)
{
	SnItem *self = SN_ITEM(data);

	CachedIcon    ci = {0};
	GError       *err = NULL;
	GdkPaintable *paintable;

	GDBusProxy *proxy = g_dbus_proxy_new_for_bus_finish(res, &err);

	if (err != NULL) {
		g_warning("Failed to construct gdbusproxy for snitem: %s\n",
		          err->message);
		g_error_free(err);
		g_object_unref(self);
		return;
	}

	self->proxy = proxy;
	self->proxy_id = g_signal_connect(self->proxy,
	                                  "g-signal",
	                                  G_CALLBACK(proxy_signal_handler),
	                                  self);

	add_icontheme_path(proxy);
	select_icon_source(self);

	switch (self->icon_source) {
	case ICON_SOURCE_NAME:
		paintable = get_paintable_from_name(self->iconname,
		                                    self->iconsize);

		strcpy(ci.iconname, self->iconname);
		ci.icondata = paintable;
		break;
	case ICON_SOURCE_PIXMAP:
		paintable = get_paintable_from_data(self->iconpixmap,
		                                    self->iconsize);

		ci.iconpixmap = g_variant_ref(self->iconpixmap);
		ci.icondata = paintable;
		break;
	case ICON_SOURCE_PATH:
		paintable = get_paintable_from_path(self->iconpath,
		                                    self->iconsize);

		strcpy(ci.iconpath, self->iconpath);
		ci.icondata = paintable;
		break;
	case ICON_SOURCE_UNKNOWN:
		paintable = get_paintable_from_name(self->iconname,
		                                    self->iconsize);

		strcpy(ci.iconname, self->iconname);
		ci.icondata = paintable;
		break;
	default:
		g_assert_not_reached();
		break;
	}

	g_array_append_val(self->cachedicons, ci);
	gtk_image_set_from_paintable(GTK_IMAGE(self->image), paintable);

	self->lclick_id = g_signal_connect(self->lclick,
	                                   "pressed",
	                                   G_CALLBACK(leftclick_handler),
	                                   self);

	connect_to_menu(self);

	g_object_unref(self);
}

static void
sn_item_notify_closed(GtkPopover *popover, void *data)
{
	SnItem *self = SN_ITEM(data);
	g_object_set(self, "menuvisible", false, NULL);
}

static void
sn_item_measure(GtkWidget     *widget,
                GtkOrientation orientation,
                int            for_size,
                int           *minimum,
                int           *natural,
                int           *minimum_baseline,
                int           *natural_baseline)
{
	SnItem *self = SN_ITEM(widget);

	switch (orientation) {
	case GTK_ORIENTATION_HORIZONTAL:
		*natural = self->iconsize;
		*minimum = self->iconsize;
		*minimum_baseline = -1;
		*natural_baseline = -1;
		break;
	case GTK_ORIENTATION_VERTICAL:
		*natural = self->iconsize;
		*minimum = self->iconsize;
		*minimum_baseline = -1;
		*natural_baseline = -1;
		break;
	}
}

static void
sn_item_size_allocate(GtkWidget *widget, int width, int height, int baseline)
{
	SnItem *self = SN_ITEM(widget);

	gtk_widget_size_allocate(self->image,
	                         &(GtkAllocation){0, 0, width, height},
	                         -1);

	gtk_popover_present(GTK_POPOVER(self->popovermenu));
}

static void
sn_item_set_property(GObject      *object,
                     unsigned int  property_id,
                     const GValue *value,
                     GParamSpec   *pspec)
{
	SnItem *self = SN_ITEM(object);

	switch (property_id) {
	case PROP_BUSNAME:
		self->busname = (g_value_dup_string(value));
		break;
	case PROP_BUSOBJ:
		self->busobj = (g_value_dup_string(value));
		break;
	case PROP_ICONSIZE:
		self->iconsize = g_value_get_int(value);
		break;
	case PROP_DBUSMENU:
		self->dbusmenu = g_value_get_object(value);
		break;
	case PROP_MENUVISIBLE:
		self->menu_visible = g_value_get_boolean(value);
		break;
	default:
		g_assert_not_reached();
		break;
	}
}

static void
sn_item_get_property(GObject     *object,
                     unsigned int property_id,
                     GValue      *value,
                     GParamSpec  *pspec)
{
	SnItem *self = SN_ITEM(object);

	switch (property_id) {
	case PROP_BUSNAME:
		g_value_set_string(value, self->busname);
		break;
	case PROP_BUSOBJ:
		g_value_set_string(value, self->busobj);
		break;
	case PROP_ICONSIZE:
		g_value_set_int(value, self->iconsize);
		break;
	case PROP_DBUSMENU:
		g_value_set_object(value, self->dbusmenu);
		break;
	case PROP_MENUVISIBLE:
		g_value_set_boolean(value, self->menu_visible);
		break;
	default:
		g_assert_not_reached();
		break;
	}
}

static void
sn_item_class_init(SnItemClass *klass)
{
	GObjectClass   *object_class = G_OBJECT_CLASS(klass);
	GtkWidgetClass *widget_class = GTK_WIDGET_CLASS(klass);

	widget_class->measure = sn_item_measure;
	widget_class->size_allocate = sn_item_size_allocate;

	object_class->set_property = sn_item_set_property;
	object_class->get_property = sn_item_get_property;
	object_class->constructed = sn_item_constructed;
	object_class->dispose = sn_item_dispose;
	object_class->finalize = sn_item_finalize;

	obj_properties[PROP_BUSNAME] =
		g_param_spec_string("busname",
	                            NULL,
	                            NULL,
	                            NULL,
	                            G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY |
	                                    G_PARAM_STATIC_STRINGS);

	obj_properties[PROP_BUSOBJ] =
		g_param_spec_string("busobj",
	                            NULL,
	                            NULL,
	                            NULL,
	                            G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY |
	                                    G_PARAM_STATIC_STRINGS);

	obj_properties[PROP_ICONSIZE] =
		g_param_spec_int("iconsize",
	                         NULL,
	                         NULL,
	                         INT_MIN,
	                         INT_MAX,
	                         22,
	                         G_PARAM_WRITABLE | G_PARAM_CONSTRUCT_ONLY |
	                                 G_PARAM_STATIC_STRINGS);

	obj_properties[PROP_DBUSMENU] =
		g_param_spec_object("dbusmenu",
	                            NULL,
	                            NULL,
	                            SN_TYPE_DBUSMENU,
	                            G_PARAM_READWRITE | G_PARAM_STATIC_STRINGS);

	obj_properties[PROP_MENUVISIBLE] =
		g_param_spec_boolean("menuvisible",
	                             NULL,
	                             NULL,
	                             false,
	                             G_PARAM_CONSTRUCT | G_PARAM_READWRITE |
	                                     G_PARAM_STATIC_STRINGS);

	g_object_class_install_properties(object_class,
	                                  N_PROPERTIES,
	                                  obj_properties);

	signals[RIGHTCLICK] = g_signal_new("rightclick",
	                                   SN_TYPE_ITEM,
	                                   G_SIGNAL_RUN_LAST,
	                                   0,
	                                   NULL,
	                                   NULL,
	                                   NULL,
	                                   G_TYPE_NONE,
	                                   0);

	gtk_widget_class_set_css_name(widget_class, "systray-item");
}

static void
sn_item_init(SnItem *self)
{
	GtkWidget *widget = GTK_WIDGET(self);

	self->in_destruction = false;
	self->update_pending = false;

	self->cachedicons = g_array_sized_new(false, true, sizeof(CachedIcon), 4);

	g_array_set_clear_func(self->cachedicons, free_cachedicon);

	self->image = gtk_image_new();
	gtk_widget_set_parent(self->image, widget);

	self->init_menu = g_menu_new();
	self->popovermenu =
		gtk_popover_menu_new_from_model(G_MENU_MODEL(self->init_menu));
	gtk_popover_menu_set_flags(GTK_POPOVER_MENU(self->popovermenu),
	                           GTK_POPOVER_MENU_NESTED);
	gtk_popover_set_has_arrow(GTK_POPOVER(self->popovermenu), false);
	gtk_widget_set_parent(self->popovermenu, widget);

	self->lclick = gtk_gesture_click_new();
	gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(self->lclick), 1);
	gtk_widget_add_controller(widget, GTK_EVENT_CONTROLLER(self->lclick));
	self->rclick = gtk_gesture_click_new();
	gtk_gesture_single_set_button(GTK_GESTURE_SINGLE(self->rclick), 3);
	gtk_widget_add_controller(widget, GTK_EVENT_CONTROLLER(self->rclick));

	g_signal_connect(self->popovermenu,
	                 "closed",
	                 G_CALLBACK(sn_item_notify_closed),
	                 self);
}

static void
sn_item_constructed(GObject *obj)
{
	SnItem *self = SN_ITEM(obj);

	GDBusNodeInfo *nodeinfo = g_dbus_node_info_new_for_xml(STATUSNOTIFIERITEM_XML,
	                                                       NULL);

	g_dbus_proxy_new_for_bus(G_BUS_TYPE_SESSION,
	                         G_DBUS_PROXY_FLAGS_NONE,
	                         nodeinfo->interfaces[0],
	                         self->busname,
	                         self->busobj,
	                         "org.kde.StatusNotifierItem",
	                         NULL,
	                         proxy_ready_handler,
	                         g_object_ref(self));

	g_dbus_node_info_unref(nodeinfo);

	G_OBJECT_CLASS(sn_item_parent_class)->constructed(obj);
}

static void
sn_item_dispose(GObject *obj)
{
	SnItem *self = SN_ITEM(obj);

	self->in_destruction = true;

	g_clear_signal_handler(&self->lclick_id, self->lclick);
	g_clear_signal_handler(&self->rclick_id, self->rclick);
	g_clear_signal_handler(&self->proxy_id, self->proxy);
	g_clear_signal_handler(&self->popup_id, self->dbusmenu);

	if (self->dbusmenu != NULL) {
		// Unref will be called from sndbusmenu dispose function
		g_object_ref(self);

		g_object_unref(self->dbusmenu);
		self->dbusmenu = NULL;
	}

	if (self->proxy != NULL) {
		g_object_unref(self->proxy);
		self->proxy = NULL;
	}

	if (self->popovermenu != NULL) {
		gtk_widget_unparent(self->popovermenu);
		self->popovermenu = NULL;
		g_object_unref(self->init_menu);
		self->init_menu = NULL;
	}

	if (self->cachedicons) {
		g_array_unref(self->cachedicons);
		self->cachedicons = NULL;
	}

	if (self->image != NULL) {
		gtk_widget_unparent(self->image);
		self->image = NULL;
	}

	if (self->iconpixmap != NULL) {
		g_variant_unref(self->iconpixmap);
		self->iconpixmap = NULL;
	}

	G_OBJECT_CLASS(sn_item_parent_class)->dispose(obj);
}

static void
sn_item_finalize(GObject *object)
{
	SnItem *self = SN_ITEM(object);

	g_free(self->busname);
	g_free(self->busobj);

	G_OBJECT_CLASS(sn_item_parent_class)->finalize(object);
}

/* PUBLIC METHODS */
void
sn_item_clear_menu_model(SnItem *self)
{
	GMenuModel     *menumodel;
	GtkPopoverMenu *popovermenu;

	g_return_if_fail(SN_IS_ITEM(self));

	if (self->popovermenu == NULL)
		return;

	popovermenu = GTK_POPOVER_MENU(self->popovermenu);
	menumodel = G_MENU_MODEL(self->init_menu);

	gtk_popover_menu_set_menu_model(popovermenu, menumodel);
}

void
sn_item_clear_actiongroup(SnItem *self, const char *prefix)
{
	g_return_if_fail(SN_IS_ITEM(self));

	gtk_widget_insert_action_group(GTK_WIDGET(self), prefix, NULL);
}

gboolean
sn_item_get_popover_visible(SnItem *self)
{
	g_return_val_if_fail(SN_IS_ITEM(self), true);

	return self->menu_visible;
}

char *
sn_item_get_busname(SnItem *self)
{
	g_return_val_if_fail(SN_IS_ITEM(self), NULL);

	if (self->busname)
		return g_strdup(self->busname);
	else
		return NULL;
}

void
sn_item_set_actiongroup(SnItem *self, const char *prefix, GSimpleActionGroup *group)
{
	g_return_if_fail(SN_IS_ITEM(self));
	g_return_if_fail(G_IS_SIMPLE_ACTION_GROUP(group));

	gtk_widget_insert_action_group(GTK_WIDGET(self),
	                               prefix,
	                               G_ACTION_GROUP(group));
}

void
sn_item_set_menu_model(SnItem *self, GMenu *menu)
{
	g_return_if_fail(SN_IS_ITEM(self));
	g_return_if_fail(G_IS_MENU(menu));

	if (self->popovermenu == NULL)
		return;

	gtk_popover_menu_set_menu_model(GTK_POPOVER_MENU(self->popovermenu),
	                                G_MENU_MODEL(menu));
}

SnItem *
sn_item_new(const char *busname, const char *busobj, int iconsize)
{
	return g_object_new(SN_TYPE_ITEM,
	                    "busname",
	                    busname,
	                    "busobj",
	                    busobj,
	                    "iconsize",
	                    iconsize,
	                    NULL);
}
/* PUBLIC METHODS */
