#!/usr/bin/env bash

# LOGFILE="analysis_571412.log"
LOGFILE=$1
FIG_PATH="output/figures/"    
MOD_PATH="data/"    
REFRESH=10           # seconds
TITLE="GCRMN ANALYSIS STATUS"
WIDTH=150
PAD=$(( (WIDTH - ${#TITLE}) / 2 ))

# Unicode border pieces
# TL="╭"   # top-left
# TR="╮"   # top-right
# BL="╰"   # bottom-left
# BR="╯"   # bottom-right
# HL="─"   # horizontal line
# VL="│"   # vertical line
TL="╔"
TR="╗"
BL="╚"
BR="╝"
HL="═"
VL="║"
VHL="╠"
VHR="╣"
#HL="="

while true; do
    clear
    # Draw top border
    # printf "%s" "$TL"
    # printf "%${WIDTH}s" "" | sed "s/ /$HL/g"
    # printf "%s\n" "$TR"
    printf "%s%s%s\n" "$TL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$TR"
    printf "%s%*s%s%*s%s\n" "$VL" "$PAD" "" "$TITLE" "$((WIDTH - PAD - ${#TITLE}))" "" "$VL"
    printf "%s%s%s\n" "$VHL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$VHR"

    printf "%s %-*s %s\n" "$VL" "$((WIDTH-2))" "Tail of $LOGFILE" "$VL"
    printf "%s %-*s %s\n" "$VL" "$((WIDTH-2))" "" "$VL"
    LOG_OUT=$(tail -n 10 "$LOGFILE" | sed 's/.*\r//')
    # Body lines
    echo "$LOG_OUT" | while IFS= read -r line; do
        # printf "%s %-${WIDTH}s %s\n" "$VL" "$line" "$VL"
        # printf "%s %-*s%s\n" "$VL" "$((WIDTH-1))" "$line" "$VL"
        printf "%s%-*s%s\n" "$VL" "$WIDTH" "$line" "$VL"
    done

    printf "%s%s%s\n" "$VHL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$VHR"
    printf "%s %-*s %s\n" "$VL" "$((WIDTH-2))" "Model listing: $MOD_PATH" "$VL"
    printf "%s %-*s %s\n" "$VL" "$((WIDTH-2))" "" "$VL"
    OUT=$(ls -lath "$MOD_PATH" | grep "mod_" | head)
    # Body lines
    echo "$OUT" | while IFS= read -r line; do
        # printf "%s %-${WIDTH}s %s\n" "$VL" "$line" "$VL"
        printf "%s%-*s%s\n" "$VL" "$WIDTH" "$line" "$VL"
    done

    printf "%s%s%s\n" "$VHL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$VHR"
    printf "%s %-*s %s\n" "$VL" "$((WIDTH-2))" "Model number: $MOD_PATH" "$VL"
    printf "%s %-*s %s\n" "$VL" "$((WIDTH-2))" "" "$VL"
    OUT=$(ls -lath "$MOD_PATH" | grep "mod_" | wc)
    echo "$OUT" | while IFS= read -r line; do
        # printf "%s%-*s%s\n" "$VL" "$WIDTH" "$line" "$VL"
        printf "%s%-*s%s\n" "$VL" "$WIDTH" "$line" "$VL"
    done
    
    printf "%s%s%s\n" "$VHL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$VHR"
    printf "%s %-*s %s\n" "$VL" "$((WIDTH-2))" "Figure directory listing: $FIG_PATH" "$VL"
    printf "%s %-*s %s\n" "$VL" "$((WIDTH-2))" "" "$VL"
    OUT=$(ls -lath "$FIG_PATH" | head)
    echo "$OUT" | while IFS= read -r line; do
        # printf "%s %-${WIDTH}s %s\n" "$VL" "$line" "$VL"
        printf "%s%-*s%s\n" "$VL" "$WIDTH" "$line" "$VL"
    done

    printf "%s%s%s\n" "$VHL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$VHR"
    # ls "$MOD_PATH" | grep "mod_" | wc
    # echo
    # echo ">> Directory listing: $FIG_PATH"
    # echo "----------------------"
    # ls -lath "$FIG_PATH" | head
    # echo
    OUT="(Updated: $(date))"
    echo "$OUT" | while IFS= read -r line; do
        # printf "%s %-${WIDTH}s %s\n" "$VL" "$line" "$VL"
        printf "%s%-*s%s\n" "$VL" "$WIDTH" "$line" "$VL"
    done
    printf "%s%s%s\n" "$BL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$BR"

    sleep "$REFRESH"
done
