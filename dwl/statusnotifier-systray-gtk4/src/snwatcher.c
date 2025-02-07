/*
 *   SPDX-FileCopyrightText: 2024 Janne Veteläinen <janne.vetelainen@elisanet.fi>
 *
 *   SPDX-License-Identifier: GPL-3.0-only
 */

#include "snwatcher.h"

#include <gio/gio.h>
#include <glib-object.h>
#include <glib.h>

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

struct _SnWatcher {
	GObject parent_instance;

	GDBusConnection *conn;
	GArray          *tracked_items;
	int              owner_id;
	int              obj_reg_id;
	int              sig_sub_id;
};

G_DEFINE_FINAL_TYPE(SnWatcher, sn_watcher, G_TYPE_OBJECT)

enum
{
	PROP_TRACKEDITEMS = 1,
	N_PROPERTIES
};

enum
{
	TRAYITEM_REGISTERED,
	TRAYITEM_UNREGISTERED,
	LAST_SIGNAL
};

static GParamSpec  *obj_properties[N_PROPERTIES] = {NULL};
static unsigned int signals[LAST_SIGNAL];

static void sn_watcher_dispose (GObject *obj);

static void bus_call_method_handler (GDBusConnection       *conn,
                                     const char            *sender,
                                     const char            *object_path,
                                     const char            *interface_name,
                                     const char            *method_name,
                                     GVariant              *parameters,
                                     GDBusMethodInvocation *invocation,
                                     void                  *data);

static GVariant *bus_get_prop_handler (GDBusConnection *conn,
                                       const char      *sender,
                                       const char      *object_path,
                                       const char      *interface_name,
                                       const char      *property_name,
                                       GError         **err,
                                       void            *data);

static GDBusInterfaceVTable interface_vtable = {bus_call_method_handler,
                                                bus_get_prop_handler,
                                                NULL};

static int
find_tracked_item(const GArray *arr, const char *busname)
{
	TrackedItem *ti;
	int          i;
	bool         match = false;

	// Newest item is most likely first removed
	for (i = arr->len - 1; i >= 0; i--) {
		ti = &g_array_index(arr, TrackedItem, i);
		if (strcmp(ti->busname, busname) == 0) {
			match = true;
			break;
		}
	}

	if (match)
		return i;
	else
		return -1;
}

static void
register_item(const char *busname, const char *busobj, SnWatcher *self)
{
	TrackedItem ti;
	GError     *err = NULL;
	size_t      len_busname, len_busobj;

	// Check if we are already tracking this item
	if (find_tracked_item(self->tracked_items, busname) >= 0) {
		return;
	}

	len_busname = strlen(busname);
	len_busobj = strlen(busobj);

	if (len_busname > SN_BUS_NAME_MAX || len_busobj > SN_BUS_PATH_MAX) {
		g_warning("Received too long bus name or path!");
		return;
	}

	g_debug("Registering %s", busname);

	memcpy(ti.busname, busname, len_busname + 1);
	memcpy(ti.busobj, busobj, len_busname + 1);
	g_array_append_val(self->tracked_items, ti);

	g_signal_emit(self, signals[TRAYITEM_REGISTERED], 0, busname, busobj);

	// Dbus signal is emitted only to conform to the specification.
	// We don't use this ourselves.
	g_dbus_connection_emit_signal(self->conn,
	                              NULL,
	                              "/StatusNotifierWatcher",
	                              "org.kde.StatusNotifierWatcher",
	                              "StatusNotifierItemRegistered",
	                              g_variant_new("(s)", busname),
	                              &err);
	if (err != NULL) {
		g_warning("%s", err->message);
		g_error_free(err);
	}
}

static void
unregister_item(TrackedItem *ti, SnWatcher *self)
{
	GError *err = NULL;

	g_debug("Unregistering %s", ti->busname);
	g_signal_emit(self, signals[TRAYITEM_UNREGISTERED], 0, ti->busname);

	// Dbus signal is emitted only to conform to the specification.
	// We don't use this ourselves.
	g_dbus_connection_emit_signal(self->conn,
	                              NULL,
	                              "/StatusNotifierWatcher",
	                              "org.kde.StatusNotifierWatcher",
	                              "StatusNotifierItemUnregistered",
	                              g_variant_new("(s)", ti->busname),
	                              &err);

	if (err != NULL) {
		g_warning("%s", err->message);
		g_error_free(err);
	}
}

static void
unregister_all(SnWatcher *self)
{
	TrackedItem *ti;

	for (unsigned int i = 0; i < self->tracked_items->len; i++) {
		ti = &g_array_index(self->tracked_items, TrackedItem, i);
		unregister_item(ti, self);
	}
}

static GVariant *
bus_get_prop_handler(GDBusConnection *conn,
                     const char      *sender,
                     const char      *object_path,
                     const char      *interface_name,
                     const char      *property_name,
                     GError         **err,
                     void            *data)
{
	SnWatcher *self = SN_WATCHER(data);

	GVariant *as;

	GVariantBuilder *builder;
	TrackedItem     *ti;

	if (strcmp(property_name, "ProtocolVersion") == 0) {
		return g_variant_new("i", 0);

	} else if (strcmp(property_name, "IsStatusNotifierHostRegistered") == 0)
	{
		return g_variant_new("b", true);

	} else if (strcmp(property_name, "RegisteredStatusNotifierItems") == 0)
	{
		if (self->tracked_items->len == 0)
			return g_variant_new("as", NULL);

		builder = g_variant_builder_new(G_VARIANT_TYPE_ARRAY);
		for (unsigned int i = 0; i < self->tracked_items->len; i++) {
			ti = &g_array_index(self->tracked_items, TrackedItem, i);
			g_variant_builder_add_value(builder,
			                            g_variant_new_string(ti->busname));
		}
		as = g_variant_builder_end(builder);

		g_variant_builder_unref(builder);
		return as;

	} else {
		g_set_error(err,
		            G_DBUS_ERROR,
		            G_DBUS_ERROR_UNKNOWN_PROPERTY,
		            "Unknown property '%s'.",
		            property_name);

		return NULL;
	}
}

static void
bus_call_method_handler(GDBusConnection       *conn,
                        const char            *sender,
                        const char            *obj_path,
                        const char            *iface_name,
                        const char            *method_name,
                        GVariant              *params,
                        GDBusMethodInvocation *invoc,
                        void                  *data)
{
	SnWatcher *self = SN_WATCHER(data);

	if (strcmp(method_name, "RegisterStatusNotifierItem") == 0) {
		const char *param;
		const char *busobj;
		const char *registree_name;

		g_variant_get(params, "(&s)", &param);

		if (g_str_has_prefix(param, "/"))
			busobj = param;
		else
			busobj = "/StatusNotifierItem";

		if (g_str_has_prefix(param, ":"))
			registree_name = param;
		else
			registree_name = sender;

		register_item(registree_name, busobj, self);
		g_dbus_method_invocation_return_value(invoc, NULL);
	} else {
		g_dbus_method_invocation_return_dbus_error(invoc,
		                                           "org.freedesktop.DBus.Error.UnknownMethod",
		                                           "Unknown method");
	}
}

static void
bus_monitor(GDBusConnection *conn,
            const char      *sender,
            const char      *objpath,
            const char      *iface_name,
            const char      *signame,
            GVariant        *params,
            void            *data)
{
	SnWatcher *self = SN_WATCHER(data);

	int          match_index;
	TrackedItem *ti;
	const char  *name;
	const char  *old_owner;
	const char  *new_owner;

	if (strcmp(signame, "NameOwnerChanged") == 0) {
		if (self->tracked_items->len == 0)
			return;

		g_variant_get(params, "(&s&s&s)", &name, &old_owner, &new_owner);

		if (strcmp(new_owner, "") != 0)
			return;

		match_index = find_tracked_item(self->tracked_items, name);

		if (match_index < 0)
			return;

		ti = &g_array_index(self->tracked_items, TrackedItem, match_index);

		unregister_item(ti, self);
		g_array_remove_index_fast(self->tracked_items, match_index);
	}
}

static void
bus_acquired_handler(GDBusConnection *conn, const char *busname, void *data)
{
	SnWatcher *self = SN_WATCHER(data);

	GError        *err = NULL;
	GDBusNodeInfo *nodeinfo =
		g_dbus_node_info_new_for_xml(STATUSNOTIFIERWATCHER_XML, NULL);

	self->conn = conn;

	self->obj_reg_id =
		g_dbus_connection_register_object(self->conn,
	                                          "/StatusNotifierWatcher",
	                                          nodeinfo->interfaces[0],
	                                          &interface_vtable,
	                                          self,
	                                          NULL,
	                                          &err);

	if (err != NULL) {
		g_error("%s", err->message);
		g_error_free(err);
		exit(EXIT_FAILURE);
	}

	self->sig_sub_id =
		g_dbus_connection_signal_subscribe(self->conn,
	                                           NULL, // All senders
	                                           "org.freedesktop.DBus",
	                                           "NameOwnerChanged",
	                                           NULL, // All obj paths
	                                           NULL, // All arg0s
	                                           G_DBUS_SIGNAL_FLAGS_NONE,
	                                           bus_monitor,
	                                           self,
	                                           NULL);

	g_dbus_node_info_unref(nodeinfo);
}

static void
bus_name_acquired_handler(GDBusConnection *conn, const char *busname, void *data)
{
	SnWatcher *self = SN_WATCHER(data);

	GError *err = NULL;

	g_dbus_connection_emit_signal(self->conn,
	                              NULL,
	                              "/StatusNotifierWatcher",
	                              "org.kde.StatusNotifierWatcher",
	                              "StatusNotifierHostRegistered",
	                              NULL,
	                              &err);

	if (err != NULL) {
		g_warning("%s", err->message);
		g_error_free(err);
	}
}

static void
bus_name_lost_handler(GDBusConnection *conn, const char *busname, void *data)
{
	g_error("Could not acquire %s, is another instance running?", busname);

	exit(EXIT_FAILURE);
}

GObject *
sn_watcher_constructor(GType                  type,
                       unsigned int           n_construct_properties,
                       GObjectConstructParam *construct_properties)
{
	static GObject *singleton = NULL;

	if (singleton == NULL) {
		singleton = G_OBJECT_CLASS(sn_watcher_parent_class)
		                    ->constructor(type,
		                                  n_construct_properties,
		                                  construct_properties);

		g_object_add_weak_pointer(singleton, (void *)&singleton);

		return singleton;
	}

	return g_object_ref(singleton);
}

static GVariant *
sn_watcher_compute_tracked_items(SnWatcher *self)
{
	GVariant       *snitems;
	GVariantBuilder builder;
	TrackedItem    *ti;

	g_variant_builder_init(&builder, G_VARIANT_TYPE("a(ss)"));
	for (unsigned int i = 0; i < self->tracked_items->len; i++) {
		ti = &g_array_index(self->tracked_items, TrackedItem, i);

		g_variant_builder_add(&builder, "(ss)", ti->busname, ti->busobj);
	}
	snitems = g_variant_builder_end(&builder);

	return snitems;
}

static void
sn_watcher_get_property(GObject     *object,
                        unsigned int property_id,
                        GValue      *value,
                        GParamSpec  *pspec)
{
	SnWatcher *self = SN_WATCHER(object);

	switch (property_id) {
	case PROP_TRACKEDITEMS:
		g_value_set_variant(value,
		                    sn_watcher_compute_tracked_items(self));
		break;
	default:
		g_assert_not_reached();
		break;
	}
}

static void
sn_watcher_class_init(SnWatcherClass *klass)
{
	GObjectClass *object_class = G_OBJECT_CLASS(klass);

	object_class->constructor = sn_watcher_constructor;
	object_class->get_property = sn_watcher_get_property;
	object_class->dispose = sn_watcher_dispose;

	obj_properties[PROP_TRACKEDITEMS] =
		g_param_spec_variant("tracked-items",
	                             NULL,
	                             NULL,
	                             G_VARIANT_TYPE_ARRAY,
	                             NULL,
	                             G_PARAM_READABLE | G_PARAM_STATIC_STRINGS);

	g_object_class_install_properties(object_class,
	                                  N_PROPERTIES,
	                                  obj_properties);

	signals[TRAYITEM_REGISTERED] = g_signal_new("trayitem-registered",
	                                            SN_TYPE_WATCHER,
	                                            G_SIGNAL_RUN_LAST,
	                                            0,
	                                            NULL,
	                                            NULL,
	                                            NULL,
	                                            G_TYPE_NONE,
	                                            2,
	                                            G_TYPE_STRING,
	                                            G_TYPE_STRING);

	signals[TRAYITEM_UNREGISTERED] = g_signal_new("trayitem-unregistered",
	                                              SN_TYPE_WATCHER,
	                                              G_SIGNAL_RUN_LAST,
	                                              0,
	                                              NULL,
	                                              NULL,
	                                              NULL,
	                                              G_TYPE_NONE,
	                                              1,
	                                              G_TYPE_STRING);
}

static void
sn_watcher_init(SnWatcher *self)
{
	self->tracked_items = g_array_sized_new(false,
	                                        false,
	                                        sizeof(TrackedItem),
	                                        12);

	self->owner_id = g_bus_own_name(G_BUS_TYPE_SESSION,
	                                "org.kde.StatusNotifierWatcher",
	                                G_BUS_NAME_OWNER_FLAGS_NONE,
	                                bus_acquired_handler,
	                                bus_name_acquired_handler,
	                                bus_name_lost_handler,
	                                self,
	                                NULL);

	g_debug("Created snwatcher");
}

static void
sn_watcher_dispose(GObject *obj)
{
	SnWatcher *self = SN_WATCHER(obj);

	g_debug("Disposing snwatcher");

	if (self->sig_sub_id > 0) {
		g_dbus_connection_signal_unsubscribe(self->conn,
		                                     self->sig_sub_id);
		self->sig_sub_id = 0;
	}

	if (self->obj_reg_id > 0) {
		g_dbus_connection_unregister_object(self->conn,
		                                    self->obj_reg_id);
		self->obj_reg_id = 0;
	}

	if (self->tracked_items != NULL) {
		unregister_all(self);
		g_array_unref(self->tracked_items);
		self->tracked_items = NULL;
	}

	if (self->owner_id > 0) {
		g_bus_unown_name(self->owner_id);
		self->owner_id = 0;
		self->conn = NULL;
	}

	G_OBJECT_CLASS(sn_watcher_parent_class)->dispose(obj);
}

/* PUBLIC METHODS */
// This is mainly for if this class would be converted to a standalone program
// run as dbus service
GVariant *
sn_watcher_get_tracked_items_variant(SnWatcher *watcher)
{
	GVariant *snitems;

	g_return_val_if_fail(SN_IS_WATCHER(watcher), NULL);

	snitems = sn_watcher_compute_tracked_items(watcher);
	snitems = g_variant_ref_sink(snitems);

	return snitems;
}

GArray *
sn_watcher_get_tracked_items(SnWatcher *watcher)
{
	g_return_val_if_fail(SN_IS_WATCHER(watcher), NULL);

	return g_array_ref(watcher->tracked_items);
}

SnWatcher *
sn_watcher_get_default(void)
{
	return g_object_new(SN_TYPE_WATCHER, NULL);
}
/* PUBLIC METHODS */
