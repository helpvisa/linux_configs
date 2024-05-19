#!/bin/sh

# A dwm_bar function to read the battery level and status
# Joe Standring <git@joestandring.com>
# GNU GPLv3

dwm_battery () {
    # Change BAT1 to whatever your battery is identified as. Typically BAT0 or BAT1
    CHARGE=$(cat /sys/class/power_supply/BAT0/capacity)
    STATUS=$(cat /sys/class/power_supply/BAT0/status)
    CURRENT=$(cat /sys/class/power_supply/BAT0/current_now)
    VOLTAGE=$(cat /sys/class/power_supply/BAT0/voltage_now)

    # divide
    CURRENT=$(bc <<< "scale=5;$CURRENT/1000000")
    VOLTAGE=$(bc <<< "scale=5;$VOLTAGE/1000000")

    printf "%s" "$SEP1"
    if [ "$IDENTIFIER" = "unicode" ]; then
        if [ "$STATUS" = "Charging" ]; then
	    CAPACITY=$(cat /sys/class/power_supply/BAT0/charge_full)
	    LEFT=$(cat /sys/class/power_supply/BAT0/charge_now)
	    LEFT=$(bc <<< "$CAPACITY - $LEFT")
	    LEFT=$(bc <<< "scale=5;$LEFT/1000000")
	    TIME_LEFT=$(bc <<< "scale=2;$LEFT/$CURRENT")
            printf "plugged in: %s%% with about %s hours until full" "$CHARGE" "$TIME_LEFT"
        else
	    CAPACITY=$(cat /sys/class/power_supply/BAT0/charge_now)
	    CAPACITY=$(bc <<< "scale=5;$CAPACITY/1000000")
	    TIME_LEFT=$(bc <<< "scale=2;$CAPACITY/$CURRENT")
            printf "battery: %s%% with about %s hours remaining" "$CHARGE" "$TIME_LEFT"
        fi
    else
        printf "BAT %s: %s%%" "$STATUS" "$CHARGE"
    fi
    printf "%s\n" "$SEP2"
}

dwm_battery

