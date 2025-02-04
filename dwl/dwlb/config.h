#define HEX_COLOR(hex)				\
	{ .red   = ((hex >> 24) & 0xff) * 257,	\
	  .green = ((hex >> 16) & 0xff) * 257,	\
	  .blue  = ((hex >> 8) & 0xff) * 257,	\
	  .alpha = (hex & 0xff) * 257 }

// use ipc functionality
static bool ipc = true;
// initially hide all bars
static bool hidden = false;
// initially draw all bars at the bottom
static bool bottom = false;
// hide vacant tags
static bool hide_vacant = false;
// vertical pixel padding above and below text
static uint32_t vertical_padding = 1;
// allow in-line color commands in status text
static bool status_commands = true;
// center title text
static bool center_title = false;
// use title space as status text element
static bool custom_title = false;
// title color use active colors
static bool active_color_title = true;
// scale
static uint32_t buffer_scale = 2;
// font
static char *fontstr = "Input Mono:size=10";
// tag names
static char *tags_names[] = { "1", "2", "3", "4", "5", "6", "7", "8", "9" };

// set 16-bit colors for bar
// use either pixman_color_t struct or HEX_COLOR macro for 8-bit colors
static pixman_color_t active_fg_color = HEX_COLOR(0xeeeeeeff);
static pixman_color_t active_bg_color = HEX_COLOR(0xbc663fff);
static pixman_color_t occupied_fg_color = HEX_COLOR(0xeeeeeeff);
static pixman_color_t occupied_bg_color = HEX_COLOR(0x3b546aff);
static pixman_color_t inactive_fg_color = HEX_COLOR(0xbbbbbbff);
static pixman_color_t inactive_bg_color = HEX_COLOR(0x3b546aff);
static pixman_color_t urgent_fg_color = HEX_COLOR(0x222222ff);
static pixman_color_t urgent_bg_color = HEX_COLOR(0xffac79ff);
static pixman_color_t middle_bg_color = HEX_COLOR(0x3b546aff);
static pixman_color_t middle_bg_color_selected = HEX_COLOR(0xbc663fff);
