/*
 *   SPDX-FileCopyrightText: 2024 Janne Veteläinen <janne.vetelainen@elisanet.fi>
 *
 *   SPDX-License-Identifier: GPL-3.0-only
 */

#ifndef SNWATCHER_H
#define SNWATCHER_H

#include <glib-object.h>
#include <glib.h>

G_BEGIN_DECLS

#define SN_BUS_NAME_MAX 255
#define SN_BUS_PATH_MAX 255

/**
 * TrackedItem:
 * @busname: An unique name on the session bus.
 * @busobj: A bus object path on the session bus.
 *
 * `TrackedItem` contains the information necessary to connect to
 * a StatusNotifierItem instance on the session bus.
 */
typedef struct {
	char busname[SN_BUS_NAME_MAX + 1];
	char busobj[SN_BUS_PATH_MAX + 1];
} TrackedItem;

#define SN_TYPE_WATCHER sn_watcher_get_type()
G_DECLARE_FINAL_TYPE(SnWatcher, sn_watcher, SN, WATCHER, GObject)

GArray    *sn_watcher_get_tracked_items (SnWatcher *watcher);
GVariant  *sn_watcher_get_tracked_items_variant (SnWatcher *watcher);
SnWatcher *sn_watcher_get_default (void);

#ifndef STATUSNOTIFIERWATCHER_XML
#define STATUSNOTIFIERWATCHER_XML                                                                   \
	"<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"                                              \
	"<node>\n"                                                                                  \
	"    <interface name=\"org.kde.StatusNotifierWatcher\">\n"                                  \
	"        <!-- methods -->\n"                                                                \
	"        <method name=\"RegisterStatusNotifierItem\">\n"                                    \
	"            <arg name=\"service\" type=\"s\" direction=\"in\" />\n"                        \
	"        </method>\n"                                                                       \
	"        <!-- properties -->\n"                                                             \
	"        <property name=\"IsStatusNotifierHostRegistered\" type=\"b\" access=\"read\" />\n" \
	"        <property name=\"ProtocolVersion\" type=\"i\" access=\"read\" />\n"                \
	"        <property name=\"RegisteredStatusNotifierItems\" type=\"as\" access=\"read\" />\n" \
	"        <!-- signals -->\n"                                                                \
	"        <signal name=\"StatusNotifierItemRegistered\">\n"                                  \
	"            <arg type=\"s\"/>\n"                                                           \
	"        </signal>\n"                                                                       \
	"        <signal name=\"StatusNotifierItemUnregistered\">\n"                                \
	"            <arg type=\"s\"/>\n"                                                           \
	"        </signal>\n"                                                                       \
	"        <signal name=\"StatusNotifierHostRegistered\">\n"                                  \
	"        </signal>\n"                                                                       \
	"    </interface>\n"                                                                        \
	"</node>\n"
#endif /* STATUSNOTIFIERWATCHER_XML */

G_END_DECLS

#endif /* SNWATCHER_H */
