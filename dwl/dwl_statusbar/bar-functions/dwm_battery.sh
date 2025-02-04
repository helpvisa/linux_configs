#!/bin/sh

# A dwm_bar function to read the battery level and status
# Joe Standring <git@joestandring.com>
# GNU GPLv3

dwm_battery () {
    # # Change BAT1 to whatever your battery is identified as. Typically BAT0 or BAT1
    # CHARGE=$(cat /sys/class/power_supply/BAT0/capacity)
    # STATUS=$(cat /sys/class/power_supply/BAT0/status)
    # CURRENT=$(cat /sys/class/power_supply/BAT0/current_now)
    # VOLTAGE=$(cat /sys/class/power_supply/BAT0/voltage_now)

    # # divide
    # CURRENT=$(bc <<< "scale=5;$CURRENT/1000000")
    # VOLTAGE=$(bc <<< "scale=5;$VOLTAGE/1000000")

    # printf "%s" "$SEP1"
    # if [ "$STATUS" = "Charging" ]; then
    #     CAPACITY=$(cat /sys/class/power_supply/BAT0/charge_full)
    #     LEFT=$(cat /sys/class/power_supply/BAT0/charge_now)
    #     LEFT=$(bc <<< "$CAPACITY - $LEFT")
    #     LEFT=$(bc <<< "scale=5;$LEFT/1000000")
    #     TIME_LEFT=$(bc <<< "scale=2;$LEFT/$CURRENT")
    #     printf "%s%% / ~%s h to full" "++$CHARGE" "$TIME_LEFT"
    # else
    #     CAPACITY=$(cat /sys/class/power_supply/BAT0/charge_now)
    #     CAPACITY=$(bc <<< "scale=5;$CAPACITY/1000000")
    #     TIME_LEFT=$(bc <<< "scale=2;$CAPACITY/$CURRENT")
    #     printf "%s%% / ~%s h left" "--$CHARGE" "$TIME_LEFT"
    # fi
    # printf "%s\n" "$SEP2"
    STATUS=$(acpi -b | sed "s/Battery 0: //;s/Discharging, /--/;s/Charging, /++/;s/\(remaining\|until charged\)/left/;s/Full, //;s/rate information unavailable//")
    printf "%s" "$SEP1"
    printf "%s" "$STATUS"
    printf "%s\n" "$SEP2"
}

dwm_battery

