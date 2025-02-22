[{
	"layer": "bottom",
	"position": "top",
	    "output": "DP-1",
	"height": 24,
	"modules-left": [
		"idle_inhibitor",
                "custom/windowpicker",
                "wlr/taskbar"
	],
	"modules-center": [
	],
	"modules-right": [
                "power-profiles-daemon",
		"pulseaudio",
		"network",
		"memory",
		"cpu",
		"disk",
		"clock",
		"tray"
	],
	// Modules
        "custom/windowpicker": {
            "format": " [ select window ] ",
            "on-click": "~/Scripts/sway/switch-window.sh"
        },
        "wlr/taskbar": {
            "format": "{title}",
            "on-click": "activate",
            "on-click-middle": "minimize",
            "icon-theme": "Papirus",
            "tooltip-format": "{name}",
            "icon-size": 12,
            "markup": true,
            "active-first": false
        },
	"sway/mode": {
		"format": "<span style=\"italic\">{}</span>"
	},
	"sway/scratchpad": {
		"format": "{icon} {count} on pad | ",
		"show-empty": false,
		"format-icons": ["", "->"],
		"tooltip": true,
		"tooltip-format": "{app}: {title}"
	},
	"idle_inhibitor": {
		"format": " [ {icon} ] ",
		"format-icons": {
			"activated": "idle off",
			"deactivated": "idle on"
		}
	},
	"clock": {
		"format": " [ {:%m-%d-%y - %H:%M} ] "
	},
	"tray": {
		"spacing": 4
	},
	"cpu": {
		"format": " [ cpu: {usage}% | load: {load} ] "
	},
	"memory": {
		"format": " [ mem: {}% ] "
	},
	"temperature": {
		"critical-threshold": 90,
		"format": " [ temp: {temperatureC}°C ] "
	},
	"disk": {
		"format": " [ disk: {specific_used:0.2f}TiB of {specific_total:0.2f}TiB ({specific_free:0.2f}TiB free) ] ",
		"unit": "TiB"
	},
	"network": {
		"format-wifi": " [ net @ {signalStrength}% ] ",
		"format-ethernet": " [ {ipaddr}/{cidr} ] ",
		"tooltip-format": "{essid} at {ifname} via {gwaddr}",
		"format-linked": " [ {ifname} (No IP) ] ",
		"format-disconnected": " [ no net! ] ",
		"format-alt": " [ {ifname}: {idappr}/{cidr} ] "
	},
	"pulseaudio": {
		"format": " [ vol: {volume}% | mic: {format_source} ] ",
		"format-muted": " [ vol: MUTED | mic: {format_source} ] ",
		"format-source": "{volume}%",
		"format-source-muted":"MUTED",
		"on-click": "pavucontrol"
	},
	"backlight": {
		"format": " [ bcklt: {percent}% ] "
	},
	"battery": {
		"states": {
			"warning": 20,
			"critical": 10
		},
		"format": " [ bat: {capacity}% ] "
	},
	"power-profiles-daemon": {
		"format": " [ {icon} ] ",
		"tooltip-format": "{profile}\n{driver}",
	        "tooltip": true,
		"format-icons": {
			"default": "P",
			"performance": "perf",
			"balanced": "bal",
			"power-saver": "psave"
		}
	},
        "custom/notification": {
            "tooltip": false,
            "format": " [ notif: {}{icon} ] ",
            "format-icons": {
              "notification": "<span foreground='red'>!</span>",
              "none": "",
              "dnd-notification": "x<span foreground='red'>!</span>",
              "dnd-none": "x",
              "inhibited-notification": "-<span foreground='red'>!</span>",
              "inhibited-none": "-",
              "dnd-inhibited-notification": "x-<span foreground='red'>!</span>",
              "dnd-inhibited-none": "x-"
            },
            "return-type": "json",
            "exec-if": "which swaync-client",
            "exec": "swaync-client -swb",
            "on-click": "swaync-client -t -sw",
            "on-click-right": "swaync-client -d -sw",
            "escape": true
         }
},
{
	"layer": "bottom",
	"position": "top",
	    "output": "HDMI-A-1",
	"height": 24,
	"modules-left": [
		"sway/workspaces",
		"sway/mode",
		"sway/scratchpad",
		"idle_inhibitor",
		"sway/window"
	],
	"modules-center": [
	],
	"modules-right": [
		"pulseaudio",
		"network",
		"memory",
		"cpu",
		"clock",
		"tray"
	],
	// Modules
	"sway/window": {
		"format": " [ {title} ] "
	},
	"sway/mode": {
		"format": "<span style=\"italic\">{}</span>"
	},
	"sway/scratchpad": {
		"format": "{icon} {count} on pad | ",
		"show-empty": false,
		"format-icons": ["", "->"],
		"tooltip": true,
		"tooltip-format": "{app}: {title}"
	},
	"idle_inhibitor": {
		"format": " [ {icon} ] ",
		"format-icons": {
			"activated": "idle off",
			"deactivated": "idle on"
		}
	},
	"clock": {
		"format": " [ {:%H:%M} ] "
	},
	"tray": {
		"spacing": 5
	},
	"cpu": {
		"format": " [ c: {usage}% | l: {load} ] "
	},
	"memory": {
		"format": " [ m: {}% ] "
	},
	"temperature": {
		"critical-threshold": 90,
		"format": " [ t: {temperatureC}°C ] "
	},
	"network": {
		"format-wifi": " [ n @ {signalStrength}% ] ",
		"format-ethernet": " [ {ipaddr}/{cidr} ] ",
		"tooltip-format": "{essid} at {ifname} via {gwaddr}",
		"format-linked": " [ {ifname} (No IP) ] ",
		"format-disconnected": " [ !n ] ",
		"format-alt": " [ {ifname}: {idappr}/{cidr ] |"
	},
	"pulseaudio": {
		"format": " [ v: {volume}% | m: {format_source} ] ",
		"format-muted": " [ v: M | m: {format_source} ] ",
		"format-source": "{volume}%",
		"format-source-muted":"M",
		"on-click": "pavucontrol"
	},
	"power-profiles-daemon": {
		"format": " [ {icon} ] ",
		"tooltip-format": "{profile}\n{driver}",
	        "tooltip": true,
		"format-icons": {
			"default": "?",
			"performance": "p",
			"balanced": "b",
			"power-saver": "s"
		}
	},
        "custom/notification": {
            "tooltip": false,
            "format": " [ notif: {}{icon} ] ",
            "format-icons": {
              "notification": "<span foreground='red'>!</span>",
              "none": "",
              "dnd-notification": "x<span foreground='red'>!</span>",
              "dnd-none": "x",
              "inhibited-notification": "-<span foreground='red'>!</span>",
              "inhibited-none": "-",
              "dnd-inhibited-notification": "x-<span foreground='red'>!</span>",
              "dnd-inhibited-none": "x-"
            },
            "return-type": "json",
            "exec-if": "which swaync-client",
            "exec": "swaync-client -swb",
            "on-click": "swaync-client -t -sw",
            "on-click-right": "swaync-client -d -sw",
            "escape": true
         }
}]
