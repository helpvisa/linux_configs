/* Taken from https://github.com/djpohly/dwl/issues/466 */
#define COLOR(hex)    { ((hex >> 24) & 0xFF) / 255.0f, \
                        ((hex >> 16) & 0xFF) / 255.0f, \
                        ((hex >> 8) & 0xFF) / 255.0f, \
                        (hex & 0xFF) / 255.0f }
/* appearance */
static const int sloppyfocus               = 1;  /* focus follows mouse */
static const int bypass_surface_visibility = 0;  /* 1 means idle inhibitors will disable idle tracking even if it's surface isn't visible  */
static const unsigned int borderpx         = 4;  /* border pixel of windows */
static const int showbar                   = 1;
static const int topbar                    = 1;
static const char *fonts[]                 = { "Input Mono:size=10" };
static const float rootcolor[]             = COLOR(0x333135ff);
static const int trayspacing               = 2; /* Spacing between icons in system tray */
static const int traymargins               = 1; /* System tray inner margins */
static const float bordercolor[]           = COLOR(0x3b546aff);
static const float focuscolor[]            = COLOR(0xbc663fff);
static const float urgentcolor[]           = COLOR(0xffac79ff);
/* This conforms to the xdg-protocol. Set the alpha to zero to restore the old behavior */
static const float fullscreen_bg[]         = {0.41f, 0.22f, 0.19f, 1.0f}; /* You can also use glsl colors */
static uint32_t colors[][3]                = {
	/*               fg          bg          border    */
	[SchemeNorm] = { 0xbbbbbbff, 0x3b546aff, 0x3b546aff },
	[SchemeSel]  = { 0xeeeeeeff, 0xbc663fff, 0xbc663fff },
	[SchemeUrg]  = { 0x222222ff, 0xffac79ff, 0xffac79ff },
};

/* tagging - TAGCOUNT must be no greater than 31 */
static char *tags[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

/* logging */
static int log_level = WLR_ERROR;

/* NOTE: ALWAYS keep a rule declared even if you don't use rules (e.g leave at least one example) */
static const Rule rules[] = {
	/* app_id                         title       tags mask     isfloating   monitor */
	/* examples: */
	{ "qalculator",                   NULL,       0,            1,           -1 }, /* Start on currently visible tags floating, not tiled */
	{ "gcolor3",                      NULL,       0,            1,           -1 }, /* Start on currently visible tags floating, not tiled */
	{ "steam",                        NULL,       0,            1,           -1 }, /* Start on currently visible tags floating, not tiled */
	{ "org.gnome.Nautilus",           NULL,       0,            1,           -1 }, /* Start on currently visible tags floating, not tiled */
	{ "org.gnome.NautilusPreviewer",  NULL,       0,            1,           -1 }, /* Start on currently visible tags floating, not tiled */
	{ "org.pulseaudio.pavucontrol",   NULL,       0,            1,           -1 }, /* Start on currently visible tags floating, not tiled */
	{ "Godot",                        NULL,       0,            1,           -1 }, /* Start on currently visible tags floating, not tiled */
};

/* layout(s) */
static const Layout layouts[] = {
	/* symbol     arrange function */
	{ "[]=",      tile },
	{ "---",      NULL },    /* no layout function means floating behavior */
	{ "[M]",      monocle },
};

/* monitors */
/* (x=-1, y=-1) is reserved as an "autoconfigure" monitor position indicator
 * WARNING: negative values other than (-1, -1) cause problems with Xwayland clients
 * https://gitlab.freedesktop.org/xorg/xserver/-/issues/899
*/
/* NOTE: ALWAYS add a fallback rule, even if you are completely sure it won't be used */
static const MonitorRule monrules[] = {
	/* name       mfact  nmaster scale layout       rotate/reflect                x    y */
	/* example of a HiDPI laptop monitor:
	{ "eDP-1",    0.5f,  1,      2,    &layouts[0], WL_OUTPUT_TRANSFORM_NORMAL,   -1,  -1 },
	*/
	/* defaults */
	{ "eDP-1",    0.55f, 1,      2.25, &layouts[0], WL_OUTPUT_TRANSFORM_NORMAL,   -1,  -1 },
	{ NULL,       0.55f, 1,      1,    &layouts[0], WL_OUTPUT_TRANSFORM_NORMAL,   -1,  -1 },
};

/* keyboard */
static const struct xkb_rule_names xkb_rules = {
	/* can specify fields: rules, model, layout, variant, options */
	/* example:
	.options = "ctrl:nocaps",
	*/
	.options = "ctrl:nocaps,numpad:mac",
};

/* input devices */
static const InputRule inputrules[] = {
	/* name                      kbcreate                 ptrcreate      */
	/* ignore bad device - like a touchpad ;) */
	// { "BAD DEVICE",              NULL,                    NULL                },
	/* ungroup ydotool device - fixes a bug */
	{ "ydotoold virtual device", createungroupedkeyboard, createpointer       },
	/* put your touchpad name here to enable toggle touchpad */
	{ "DELL07E6:00 06CB:76AF Touchpad", createkeyboard, createtogglepointer },
	{ NULL,                      createkeyboard,          createpointer       },
};

static const int repeat_rate = 25;
static const int repeat_delay = 600;

/* Trackpad */
static const int tap_to_click = 1;
static const int tap_and_drag = 1;
static const int drag_lock = 1;
static const int natural_scrolling = 1;
static const int disable_while_typing = 1;
static const int left_handed = 0;
static const int middle_button_emulation = 0;
/* You can choose between:
LIBINPUT_CONFIG_SCROLL_NO_SCROLL
LIBINPUT_CONFIG_SCROLL_2FG
LIBINPUT_CONFIG_SCROLL_EDGE
LIBINPUT_CONFIG_SCROLL_ON_BUTTON_DOWN
*/
static const enum libinput_config_scroll_method scroll_method = LIBINPUT_CONFIG_SCROLL_2FG;

/* You can choose between:
LIBINPUT_CONFIG_CLICK_METHOD_NONE
LIBINPUT_CONFIG_CLICK_METHOD_BUTTON_AREAS
LIBINPUT_CONFIG_CLICK_METHOD_CLICKFINGER
*/
static const enum libinput_config_click_method click_method = LIBINPUT_CONFIG_CLICK_METHOD_CLICKFINGER;

/* You can choose between:
LIBINPUT_CONFIG_SEND_EVENTS_ENABLED
LIBINPUT_CONFIG_SEND_EVENTS_DISABLED
LIBINPUT_CONFIG_SEND_EVENTS_DISABLED_ON_EXTERNAL_MOUSE
*/
static const uint32_t send_events_mode = LIBINPUT_CONFIG_SEND_EVENTS_ENABLED;

/* You can choose between:
LIBINPUT_CONFIG_ACCEL_PROFILE_FLAT
LIBINPUT_CONFIG_ACCEL_PROFILE_ADAPTIVE
*/
static const enum libinput_config_accel_profile accel_profile = LIBINPUT_CONFIG_ACCEL_PROFILE_ADAPTIVE;
static const double accel_speed = 0.0;

/* You can choose between:
LIBINPUT_CONFIG_TAP_MAP_LRM -- 1/2/3 finger tap maps to left/right/middle
LIBINPUT_CONFIG_TAP_MAP_LMR -- 1/2/3 finger tap maps to left/middle/right
*/
static const enum libinput_config_tap_button_map button_map = LIBINPUT_CONFIG_TAP_MAP_LRM;

/* If you want to use the windows key for MODKEY, use WLR_MODIFIER_LOGO */
#define MODKEY WLR_MODIFIER_LOGO

#define TAGKEYS(KEY,SKEY,TAG) \
	{ MODKEY,                    KEY,            view,            {.ui = 1 << TAG} }, \
	{ MODKEY|WLR_MODIFIER_CTRL,  KEY,            toggleview,      {.ui = 1 << TAG} }, \
	{ MODKEY|WLR_MODIFIER_SHIFT, SKEY,           tag,             {.ui = 1 << TAG} }, \
	{ MODKEY|WLR_MODIFIER_CTRL|WLR_MODIFIER_SHIFT,SKEY,toggletag, {.ui = 1 << TAG} }

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd) { .v = (const char*[]){ "/bin/sh", "-c", cmd, NULL } }

/* commands */
/* launchers */
static const char *termcmd[] = { "foot", NULL };
static const char *menucmd[] = { "fuzzel", NULL };
static const char *web_browser[] = { "firefox", NULL };
static const char *files[] = { "nautilus", "--new-window", NULL };
static const char *calc[] = { "foot", "-a", "qalculator", "sh", "-c", "qalc", NULL };
/* actions */
static const char *lock_screen[] = { "/home/helpvisa/Scripts/sway/lock-and-blur.sh", NULL };
static const char *suspend[] = { "systemctl", "suspend", NULL };
static const char *screenshot[] = { "/home/helpvisa/Scripts/sway/screenshot.sh", NULL };
static const char *pick_color[] = { "/home/helpvisa/Scripts/sway/color-picker.sh", NULL };
/* power */
static const char *pp_perf[] = { "/home/helpvisa/Scripts/sway/power-perf.sh", NULL };
static const char *pp_bal[] = { "/home/helpvisa/Scripts/sway/power-bal.sh", NULL };
static const char *pp_save[] = { "/home/helpvisa/Scripts/sway/power-save.sh", NULL };
/* audio and screen hotkeys */
static const char *vol_up[] = { "/home/helpvisa/Scripts/sway/vol-up.sh", NULL };
static const char *vol_down[] = { "/home/helpvisa/Scripts/sway/vol-down.sh", NULL };
static const char *vol_mute[] = { "/home/helpvisa/Scripts/sway/vol-mute.sh", NULL };
static const char *mic_mute[] = { "/home/helpvisa/Scripts/sway/mic-mute.sh", NULL };
static const char *bright_up[] = { "/home/helpvisa/Scripts/sway/bright-up.sh", NULL };
static const char *bright_down[] = { "/home/helpvisa/Scripts/sway/bright-down.sh", NULL };
/* overlays */
static const char *view_notifs[] = { "swaync-client", "-t", "-sw", NULL };
/* deadly */
static const char *kill_launch_script[] = { "killall", "launch-dwl.sh", NULL };

static const Key keys[] = {
	/* Note that Shift changes certain key codes: c -> C, 2 -> at, etc. */
	/* modifier                  key                 function        argument */
        /* custom commands */
        /* app launchers */
        { MODKEY,                    XKB_KEY_w,          spawn,          {.v = web_browser} },
        { MODKEY,                    XKB_KEY_n,          spawn,          {.v = files} },
        { MODKEY,                    XKB_KEY_c,          spawn,          {.v = calc} },
        /* actions */
        { MODKEY,                    XKB_KEY_x,          spawn,          {.v = lock_screen} },
        { MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_X,          spawn,          {.v = suspend} },
        { 0,                         XKB_KEY_Print,      spawn,          {.v = screenshot} },
        { MODKEY,                    XKB_KEY_Print,      spawn,          {.v = pick_color} },
        /* power */
        { MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_P,          spawn,          {.v = pp_perf} },
        { MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_O,          spawn,          {.v = pp_bal} },
        { MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_I,          spawn,          {.v = pp_save} },
        /* audio and screen hotkeys */
        { 0,               XKB_KEY_XF86AudioRaiseVolume, spawn,          {.v = vol_up} },
        { 0,               XKB_KEY_XF86AudioLowerVolume, spawn,          {.v = vol_down} },
        { 0,               XKB_KEY_XF86AudioMute,        spawn,          {.v = vol_mute} },
        { 0,               XKB_KEY_XF86AudioMicMute,     spawn,          {.v = mic_mute} },
        { 0,               XKB_KEY_XF86MonBrightnessUp,  spawn,          {.v = bright_up} },
        { 0,               XKB_KEY_XF86MonBrightnessDown,spawn,          {.v = bright_down} },
        /* overlays */
        { MODKEY,                    XKB_KEY_m,          spawn,          {.v = view_notifs} },
        /* base commands */
	{ WLR_MODIFIER_ALT,          XKB_KEY_space,      spawn,          {.v = menucmd} },
	{ MODKEY,                    XKB_KEY_Return,     spawn,          {.v = termcmd} },
	{ MODKEY,                    XKB_KEY_b,          togglebar,      {0} },
	{ MODKEY,                    XKB_KEY_j,          focusstack,     {.i = +1} },
	{ MODKEY,                    XKB_KEY_k,          focusstack,     {.i = -1} },
	{ MODKEY,                    XKB_KEY_i,          incnmaster,     {.i = +1} },
	{ MODKEY,                    XKB_KEY_d,          incnmaster,     {.i = -1} },
	{ MODKEY,                    XKB_KEY_h,          setmfact,       {.f = -0.05f} },
	{ MODKEY,                    XKB_KEY_l,          setmfact,       {.f = +0.05f} },
	{ MODKEY,                    XKB_KEY_space,      zoom,           {0} },
	{ MODKEY,                    XKB_KEY_Tab,        view,           {0} },
	{ MODKEY,                    XKB_KEY_q,          killclient,     {0} },
	{ MODKEY,                    XKB_KEY_t,          setlayout,      {.v = &layouts[0]} },
	{ MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_F,          setlayout,      {.v = &layouts[1]} },
	{ MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_M,          setlayout,      {.v = &layouts[2]} },
	{ MODKEY,                    XKB_KEY_s,          setlayout,      {0} },
	{ MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_space,      togglefloating, {0} },
	{ MODKEY,                    XKB_KEY_f,        togglefullscreen, {0} },
	{ MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_U,          togglepointer,  {0} },
	{ MODKEY,                    XKB_KEY_0,          view,           {.ui = ~0} },
	{ MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_parenright, tag,            {.ui = ~0} },
	{ MODKEY,                    XKB_KEY_comma,      focusmon,       {.i = WLR_DIRECTION_LEFT} },
	{ MODKEY,                    XKB_KEY_period,     focusmon,       {.i = WLR_DIRECTION_RIGHT} },
	{ MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_less,       tagmon,         {.i = WLR_DIRECTION_LEFT} },
	{ MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_greater,    tagmon,         {.i = WLR_DIRECTION_RIGHT} },
	TAGKEYS(          XKB_KEY_1, XKB_KEY_exclam,                     0),
	TAGKEYS(          XKB_KEY_2, XKB_KEY_at,                         1),
	TAGKEYS(          XKB_KEY_3, XKB_KEY_numbersign,                 2),
	TAGKEYS(          XKB_KEY_4, XKB_KEY_dollar,                     3),
	TAGKEYS(          XKB_KEY_5, XKB_KEY_percent,                    4),
	TAGKEYS(          XKB_KEY_6, XKB_KEY_asciicircum,                5),
	TAGKEYS(          XKB_KEY_7, XKB_KEY_ampersand,                  6),
	TAGKEYS(          XKB_KEY_8, XKB_KEY_asterisk,                   7),
	TAGKEYS(          XKB_KEY_9, XKB_KEY_parenleft,                  8),
	// { MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_E,          quit,           {0} },
	{ MODKEY|WLR_MODIFIER_SHIFT, XKB_KEY_E,          spawn,          {.v = kill_launch_script} },

	/* Ctrl-Alt-Backspace and Ctrl-Alt-Fx used to be handled by X server */
	{ WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT,XKB_KEY_Terminate_Server, spawn, {.v = kill_launch_script} },
	{ WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT,XKB_KEY_Terminate_Server, quit, {0} },
	/* Ctrl-Alt-Fx is used to switch to another VT, if you don't know what a VT is
	 * do not remove them.
	 */
#define CHVT(n) { WLR_MODIFIER_CTRL|WLR_MODIFIER_ALT,XKB_KEY_XF86Switch_VT_##n, chvt, {.ui = (n)} }
	CHVT(1), CHVT(2), CHVT(3), CHVT(4), CHVT(5), CHVT(6),
	CHVT(7), CHVT(8), CHVT(9), CHVT(10), CHVT(11), CHVT(12),
};

static const Button buttons[] = {
	{ ClkLtSymbol, 0,      BTN_LEFT,   setlayout,      {.v = &layouts[0]} },
	{ ClkLtSymbol, 0,      BTN_RIGHT,  setlayout,      {.v = &layouts[2]} },
	{ ClkTitle,    0,      BTN_MIDDLE, zoom,           {0} },
	{ ClkStatus,   0,      BTN_MIDDLE, spawn,          {.v = termcmd} },
	{ ClkClient,   MODKEY, BTN_LEFT,   moveresize,     {.ui = CurMove} },
	{ ClkClient,   MODKEY, BTN_MIDDLE, togglefloating, {0} },
	{ ClkClient,   MODKEY, BTN_RIGHT,  moveresize,     {.ui = CurResize} },
	{ ClkTagBar,   0,      BTN_LEFT,   view,           {0} },
	{ ClkTagBar,   0,      BTN_RIGHT,  toggleview,     {0} },
	{ ClkTagBar,   MODKEY, BTN_LEFT,   tag,            {0} },
	{ ClkTagBar,   MODKEY, BTN_RIGHT,  toggletag,      {0} },
};
