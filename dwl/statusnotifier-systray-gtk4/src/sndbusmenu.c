/*
 *   SPDX-FileCopyrightText: 2024 Janne Veteläinen <janne.vetelainen@elisanet.fi>
 *
 *   SPDX-License-Identifier: GPL-3.0-only
 */

#include "sndbusmenu.h"

#include "snitem.h"

#include <gio/gio.h>
#include <glib-object.h>
#include <glib.h>

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <time.h>

struct _SnDbusmenu {
	GObject parent_instance;

	char               *busname;
	char               *busobj;
	SnItem             *snitem;
	GMenu              *menu;
	GSimpleActionGroup *actiongroup;
	GDBusProxy         *proxy;
	uint32_t            revision;
	gboolean            update_pending;
	gboolean            reschedule;
};

G_DEFINE_FINAL_TYPE(SnDbusmenu, sn_dbusmenu, G_TYPE_OBJECT)

enum
{
	PROP_BUSNAME = 1,
	PROP_BUSOBJ,
	PROP_SNITEM,
	N_PROPERTIES
};

enum
{
	ABOUT_TO_SHOW_HANDLED,
	LAST_SIGNAL
};

#define ACTION_NAME_MAX_LEN 32

static GParamSpec  *obj_properties[N_PROPERTIES] = {NULL};
static unsigned int signals[LAST_SIGNAL];
static const char  *actiongroup_pfx = "menuitem";
static const int    layout_update_freq = 80;

typedef struct {
	uint32_t    id;
	GDBusProxy *proxy;
} ActionActivationContext;

static void sn_dbusmenu_constructed (GObject *object);
static void sn_dbusmenu_dispose (GObject *object);
static void sn_dbusmenu_finalize (GObject *object);

static void sn_dbusmenu_get_property (GObject     *object,
                                      unsigned int property_id,
                                      GValue      *value,
                                      GParamSpec  *pspec);

static void sn_dbusmenu_set_property (GObject      *object,
                                      unsigned int  property_id,
                                      const GValue *value,
                                      GParamSpec   *pspec);

static GMenu *create_menumodel (GVariant *data, SnDbusmenu *self);

static GMenuItem *create_menuitem (int32_t     id,
                                   GVariant   *menu_data,
                                   GVariant   *submenu_data,
                                   SnDbusmenu *self);

static void
action_activated_handler(GSimpleAction           *action,
                         GVariant                *param,
                         ActionActivationContext *ctx)
{
	g_dbus_proxy_call(ctx->proxy,
	                  "Event",
	                  g_variant_new("(isvu)",
	                                ctx->id,
	                                "clicked",
	                                g_variant_new_string(""),
	                                time(NULL)),
	                  G_DBUS_CALL_FLAGS_NONE,
	                  -1,
	                  NULL,
	                  NULL,
	                  NULL);
}

static void
destroy_actx(ActionActivationContext *actx, GClosure *closure)
{
	g_free(actx);
}

static GSimpleAction *
create_action(uint32_t id, gboolean ischeckmark, SnDbusmenu *self)
{
	GSimpleAction *action;

	ActionActivationContext *ctx;
	char                     name[ACTION_NAME_MAX_LEN];

	ctx = g_new(ActionActivationContext, 1);
	ctx->id = id;
	ctx->proxy = self->proxy;
	sprintf(name, "%u", id);

	if (ischeckmark) {
		action = g_simple_action_new_stateful(name,
		                                      NULL,
		                                      g_variant_new("b", true));
	} else {
		action = g_simple_action_new(name, NULL);
	}

	g_signal_connect_data(action,
	                      "activate",
	                      G_CALLBACK(action_activated_handler),
	                      ctx,
	                      (GClosureNotify)destroy_actx,
	                      G_CONNECT_DEFAULT);

	return action;
}

static GMenuItem *
create_menuitem(int32_t     id,
                GVariant   *menuitem_data,
                GVariant   *submenuitem_data,
                SnDbusmenu *self)
{
	GMenuItem *menuitem;

	GActionMap    *actionmap = G_ACTION_MAP(self->actiongroup);
	GSimpleAction *action;
	GVariantDict   dict;
	char           detailed_name[ACTION_NAME_MAX_LEN];

	const char *has_submenu_s = NULL;
	const char *label = NULL;
	const char *toggle_type_s = NULL;
	const char *type_s = NULL;
	gboolean    has_submenu = false;
	gboolean    ischeckmark = false;
	gboolean    isenabled = true;
	gboolean    isseparator = false;
	gboolean    isvisible = true;
	gboolean    toggle_state = true;
	// gboolean       isradio = false;

	g_variant_dict_init(&dict, menuitem_data);
	g_variant_dict_lookup(&dict, "label", "&s", &label);
	g_variant_dict_lookup(&dict, "type", "&s", &type_s);
	g_variant_dict_lookup(&dict, "enabled", "b", &isenabled);
	g_variant_dict_lookup(&dict, "visible", "b", &isvisible);
	g_variant_dict_lookup(&dict, "children-display", "&s", &has_submenu_s);
	g_variant_dict_lookup(&dict, "toggle-type", "&s", &toggle_type_s);
	g_variant_dict_lookup(&dict, "toggle-state", "i", &toggle_state);
	g_variant_dict_clear(&dict);

	if (has_submenu_s != NULL && strcmp(has_submenu_s, "submenu") == 0)
		has_submenu = true;

	if (type_s != NULL && strcmp(type_s, "separator") == 0) {
		isseparator = true;
	} else if (toggle_type_s != NULL &&
	           strcmp(toggle_type_s, "checkmark") == 0)
	{
		ischeckmark = true;
	}
	/*
	 * else if (toggle_type_s && strcmp(toggle_type_s, "radio") == 0) {
	 * isradio = true;
	 * }
	 */

	if (!isvisible || isseparator)
		return NULL;

	action = create_action(id, ischeckmark, self);
	sprintf(detailed_name, "%s.%u", actiongroup_pfx, id);
	menuitem = g_menu_item_new(label, detailed_name);

	if (!isenabled)
		g_simple_action_set_enabled(action, false);
	if (ischeckmark) {
		g_simple_action_set_state(action,
		                          g_variant_new("b", toggle_state));
	}
	if (has_submenu) {
		GMenu *submenu = create_menumodel(submenuitem_data, self);
		g_menu_item_set_submenu(menuitem, G_MENU_MODEL(submenu));
		g_object_unref(submenu);
	}

	g_action_map_add_action(actionmap, G_ACTION(action));

	g_object_unref(action);
	return menuitem;
}

static GMenu *
create_menumodel(GVariant *data, SnDbusmenu *self)
{
	GMenu *menu = g_menu_new();

	GMenuItem   *menuitem;
	GVariant    *menuitem_data;
	GVariant    *menuitem_data_packed;
	GVariant    *submenu_data;
	GVariantIter iter;
	int32_t      id;

	g_variant_iter_init(&iter, data);
	while ((g_variant_iter_next(&iter, "v", &menuitem_data_packed))) {
		g_variant_get_child(menuitem_data_packed, 0, "i", &id);

		menuitem_data = g_variant_get_child_value(menuitem_data_packed,
		                                          1);

		submenu_data = g_variant_get_child_value(menuitem_data_packed,
		                                         2);

		menuitem = create_menuitem(id, menuitem_data, submenu_data, self);

		if (menuitem != NULL) {
			g_menu_append_item(menu, menuitem);
			g_object_unref(menuitem);
		}

		g_variant_unref(submenu_data);
		g_variant_unref(menuitem_data);
		g_variant_unref(menuitem_data_packed);
	}

	return menu;
}

static void
layout_update_finish(GObject *obj, GAsyncResult *res, void *udata)
{
	GDBusProxy *proxy = G_DBUS_PROXY(obj);
	SnDbusmenu *self = SN_DBUSMENU(udata);

	GVariant *layout;
	GVariant *menuitems;
	gboolean  isvisible;
	GMenu    *newmenu;

	GVariant *data = g_dbus_proxy_call_finish(proxy, res, NULL);

	if (data == NULL) {
		g_object_unref(self->snitem);
		g_object_unref(self);
		return;
	}

	layout = g_variant_get_child_value(data, 1);
	menuitems = g_variant_get_child_value(layout, 2);

	isvisible = sn_item_get_popover_visible(self->snitem);
	if (isvisible) {
		self->reschedule = true;
	} else {
		GSimpleActionGroup *newag = g_simple_action_group_new();
		sn_item_set_actiongroup(self->snitem, actiongroup_pfx, newag);
		g_object_unref(self->actiongroup);
		self->actiongroup = newag;

		newmenu = create_menumodel(menuitems, self);
		sn_item_set_menu_model(self->snitem, newmenu);
		g_object_unref(self->menu);
		self->menu = newmenu;
	}

	g_variant_unref(menuitems);
	g_variant_unref(layout);
	g_variant_unref(data);
	g_object_unref(self->snitem);
	g_object_unref(self);
}

static void
layout_update(SnDbusmenu *self)
{
	self->update_pending = false;

	g_dbus_proxy_call(self->proxy,
	                  "GetLayout",
	                  g_variant_new("(iias)", 0, -1, NULL),
	                  G_DBUS_CALL_FLAGS_NONE,
	                  -1,
	                  NULL,
	                  layout_update_finish,
	                  self);
}

static void
reschedule_update(SnItem *snitem, GParamSpec *pspec, void *data)
{
	SnDbusmenu *self = SN_DBUSMENU(data);
	gboolean    popover_visible;

	g_return_if_fail(SN_IS_ITEM(self->snitem));

	popover_visible = sn_item_get_popover_visible(snitem);
	if (popover_visible || !self->reschedule)
		return;

	self->reschedule = false;

	g_object_ref(self);
	g_object_ref(self->snitem);
	layout_update(self);
}

// Update signals are often received multiple times in row,
// we throttle update frequency to *layout_update_freq*
static void
proxy_signal_handler(GDBusProxy *proxy,
                     const char *sender,
                     const char *signal,
                     GVariant   *params,
                     void       *data)
{
	SnDbusmenu *self = SN_DBUSMENU(data);

	g_return_if_fail(SN_IS_ITEM(self->snitem));

	if (strcmp(signal, "LayoutUpdated") == 0) {
		uint32_t revision;
		int32_t  parentid;
		g_variant_get(params, "(ui)", &revision, &parentid);

		if (self->revision == UINT32_MAX || self->revision < revision)
			self->revision = revision;

		if (!self->update_pending) {
			self->update_pending = true;
			g_object_ref(self->snitem);

			g_timeout_add_once(layout_update_freq,
			                   (GSourceOnceFunc)layout_update,
			                   g_object_ref(self));
		}

	} else if (strcmp(signal, "ItemsPropertiesUpdated") == 0) {
		if (!self->update_pending) {
			self->update_pending = true;
			g_object_ref(self->snitem);

			g_timeout_add_once(layout_update_freq,
			                   (GSourceOnceFunc)layout_update,
			                   g_object_ref(self));
		}
	}
}

static void
menulayout_ready_handler(GObject *obj, GAsyncResult *res, void *data)
{
	GDBusProxy *proxy = G_DBUS_PROXY(obj);
	SnDbusmenu *self = SN_DBUSMENU(data);

	GVariant *layout;
	GVariant *menuitems;
	uint32_t  revision = 0;

	GVariant *retvariant = g_dbus_proxy_call_finish(proxy, res, NULL);

	if (retvariant == NULL) {
		g_object_unref(self);
		return;
	}

	g_variant_get_child(retvariant, 0, "u", &revision);
	layout = g_variant_get_child_value(retvariant, 1);
	menuitems = g_variant_get_child_value(layout, 2);

	self->menu = create_menumodel(menuitems, self);
	sn_item_set_menu_model(self->snitem, self->menu);

	g_variant_unref(menuitems);
	g_variant_unref(layout);
	g_variant_unref(retvariant);
	g_object_unref(self);
}

static void
about_to_show_timeout_handler(void *data)
{
	SnDbusmenu *self = SN_DBUSMENU(data);

	g_signal_emit(self, signals[ABOUT_TO_SHOW_HANDLED], 0);

	g_object_unref(self);
}

static void
about_to_show_handler(GObject *obj, GAsyncResult *res, void *data)
{
	GDBusProxy *proxy = G_DBUS_PROXY(obj);
	SnDbusmenu *self = SN_DBUSMENU(data);

	GError *err = NULL;
	int     timeout = 100;

	GVariant *val = g_dbus_proxy_call_finish(proxy, res, &err);

	// I give up trying to get nm-applet working properly.
	// Wait 2 seconds until popping the menu
	// to let it finish its business.
	if (strcmp(self->busobj,
	           "/org/ayatana/NotificationItem/nm_applet/Menu") == 0)
	{
		timeout = 2000;
	}

	// Discord generates the following error here:
	// 'G_DBUS_ERROR' 'G_DBUS_ERROR_FAILED' 'error occurred in AboutToShow'
	// We ignore it.
	if (err != NULL &&
	    !g_error_matches(err, G_DBUS_ERROR, G_DBUS_ERROR_FAILED) &&
	    g_strrstr(err->message, "error occured in AboutToShow") != 0)
	{
		g_warning("%s\n", err->message);

	} else {
		// This dbusmenu call might have triggered a menu update,
		g_timeout_add_once(timeout,
		                   about_to_show_timeout_handler,
		                   g_object_ref(self));
	}

	err != NULL ? g_error_free(err) : g_variant_unref(val);
	g_object_unref(self);
}

static void
rightclick_handler(GObject *obj, void *data)
{
	SnDbusmenu *self = SN_DBUSMENU(data);

	g_assert(SN_IS_DBUSMENU(self));
	g_dbus_proxy_call(self->proxy,
	                  "AboutToShow",
	                  g_variant_new("(i)", 0),
	                  G_DBUS_CALL_FLAGS_NONE,
	                  -1,
	                  NULL,
	                  about_to_show_handler,
	                  g_object_ref(self));
}

static void
proxy_ready_handler(GObject *obj, GAsyncResult *res, void *data)
{
	SnDbusmenu *self = SN_DBUSMENU(data);

	GError     *err = NULL;
	GDBusProxy *proxy = g_dbus_proxy_new_for_bus_finish(res, &err);

	if (err != NULL) {
		g_warning("Failed to construct gdbusproxy for menu: %s\n",
		          err->message);
		g_error_free(err);
		g_object_unref(self);
		return;
	}

	g_dbus_proxy_call(proxy,
	                  "GetLayout",
	                  g_variant_new("(iias)", 0, -1, NULL),
	                  G_DBUS_CALL_FLAGS_NONE,
	                  -1,
	                  NULL,
	                  menulayout_ready_handler,
	                  g_object_ref(self));

	g_signal_connect(proxy, "g-signal", G_CALLBACK(proxy_signal_handler), self);

	self->proxy = proxy;

	g_object_unref(self);
}

static void
sn_dbusmenu_get_property(GObject     *object,
                         unsigned int property_id,
                         GValue      *value,
                         GParamSpec  *pspec)
{
	G_OBJECT_WARN_INVALID_PROPERTY_ID(object, property_id, pspec);
}

static void
sn_dbusmenu_set_property(GObject      *object,
                         unsigned int  property_id,
                         const GValue *value,
                         GParamSpec   *pspec)
{
	SnDbusmenu *self = SN_DBUSMENU(object);

	switch (property_id) {
	case PROP_BUSNAME:
		self->busname = g_strdup(g_value_get_string(value));
		break;
	case PROP_BUSOBJ:
		self->busobj = g_strdup(g_value_get_string(value));
		break;
	case PROP_SNITEM:
		self->snitem = g_value_get_object(value);
		break;
	default:
		g_assert_not_reached();
		break;
	}
}

static void
sn_dbusmenu_class_init(SnDbusmenuClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->set_property = sn_dbusmenu_set_property;
	object_class->get_property = sn_dbusmenu_get_property;
	object_class->constructed = sn_dbusmenu_constructed;
	object_class->dispose = sn_dbusmenu_dispose;
	object_class->finalize = sn_dbusmenu_finalize;

	obj_properties[PROP_BUSNAME] =
		g_param_spec_string("busname",
	                            NULL,
	                            NULL,
	                            NULL,
	                            G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE |
	                                    G_PARAM_STATIC_STRINGS);

	obj_properties[PROP_BUSOBJ] =
		g_param_spec_string("busobj",
	                            NULL,
	                            NULL,
	                            NULL,
	                            G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE |
	                                    G_PARAM_STATIC_STRINGS);

	obj_properties[PROP_SNITEM] =
		g_param_spec_object("snitem",
	                            NULL,
	                            NULL,
	                            SN_TYPE_ITEM,
	                            G_PARAM_CONSTRUCT_ONLY | G_PARAM_WRITABLE |
	                                    G_PARAM_STATIC_STRINGS);

	g_object_class_install_properties(object_class,
	                                  N_PROPERTIES,
	                                  obj_properties);

	signals[ABOUT_TO_SHOW_HANDLED] = g_signal_new("abouttoshowhandled",
	                                              SN_TYPE_DBUSMENU,
	                                              G_SIGNAL_RUN_LAST,
	                                              0,
	                                              NULL,
	                                              NULL,
	                                              NULL,
	                                              G_TYPE_NONE,
	                                              0);
}

static void
sn_dbusmenu_init(SnDbusmenu *self)
{
	self->actiongroup = g_simple_action_group_new();
}

static void
sn_dbusmenu_constructed(GObject *obj)
{
	SnDbusmenu *self = SN_DBUSMENU(obj);

	GDBusNodeInfo *nodeinfo;

	sn_item_set_actiongroup(self->snitem, actiongroup_pfx, self->actiongroup);

	nodeinfo = g_dbus_node_info_new_for_xml(DBUSMENU_XML, NULL);

	g_dbus_proxy_new_for_bus(G_BUS_TYPE_SESSION,
	                         G_DBUS_PROXY_FLAGS_NONE,
	                         nodeinfo->interfaces[0],
	                         self->busname,
	                         self->busobj,
	                         "com.canonical.dbusmenu",
	                         NULL,
	                         proxy_ready_handler,
	                         g_object_ref(self));

	g_signal_connect(self->snitem,
	                 "notify::menuvisible",
	                 G_CALLBACK(reschedule_update),
	                 self);

	g_signal_connect(self->snitem,
	                 "rightclick",
	                 G_CALLBACK(rightclick_handler),
	                 self);

	g_dbus_node_info_unref(nodeinfo);

	G_OBJECT_CLASS(sn_dbusmenu_parent_class)->constructed(obj);
}

static void
sn_dbusmenu_dispose(GObject *obj)
{
	SnDbusmenu *self = SN_DBUSMENU(obj);

	g_debug("Disposing sndbusmenu %s %s", self->busname, self->busobj);

	if (self->proxy != NULL) {
		g_object_unref(self->proxy);
		self->proxy = NULL;
	}

	if (self->actiongroup != NULL) {
		sn_item_clear_actiongroup(self->snitem, actiongroup_pfx);
		g_object_unref(self->actiongroup);
		self->actiongroup = NULL;
	}

	if (self->menu != NULL) {
		sn_item_clear_menu_model(self->snitem);
		g_object_unref(self->menu);
		self->menu = NULL;
	}

	if (self->snitem != NULL) {
		g_object_unref(self->snitem);
		self->snitem = NULL;
	}

	G_OBJECT_CLASS(sn_dbusmenu_parent_class)->dispose(obj);
}

static void
sn_dbusmenu_finalize(GObject *obj)
{
	SnDbusmenu *self = SN_DBUSMENU(obj);

	g_free(self->busname);
	g_free(self->busobj);

	G_OBJECT_CLASS(sn_dbusmenu_parent_class)->finalize(obj);
}

/* PUBLIC METHODS */
SnDbusmenu *
sn_dbusmenu_new(const char *busname, const char *busobj, SnItem *snitem)
{
	return g_object_new(SN_TYPE_DBUSMENU,
	                    "busname",
	                    busname,
	                    "busobj",
	                    busobj,
	                    "snitem",
	                    snitem,
	                    NULL);
}
/* PUBLIC METHODS */
