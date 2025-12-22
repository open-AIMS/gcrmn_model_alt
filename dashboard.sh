#!/usr/bin/env bash

# LOGFILE="analysis_571412.log"
LOGFILE=$1
FIG_PATH="output/figures/"    
MOD_PATH="data/"    
REFRESH=10           # seconds
TITLE="GCRMN ANALYSIS STATUS"
WIDTH=180
PAD=$(( (WIDTH - ${#TITLE}) / 2 ))

# SPACER=" "   # space between columns
SPACER=""   # space between columns

# Total width of the whole two-column layout (inside)
# You already have: $WIDTH → total width
# Each block gets half
HALF_WIDTH=$(( ((WIDTH-2) / 2) - 2 ))   # minus borders
# HALF_WIDTH=$(( ((WIDTH-4) / 2) - 0 ))   # minus borders


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


# ────────────────────────────────────────────────
# Function: build a block into an array
# ────────────────────────────────────────────────
build_block () {
    local title="$1"
    local list_input="$2"
    local -n BLOCK=$3   # reference to array name passed in
    local type="$4"
    
    local TL_G="╔"  TR_G="╗"
    local BL_G="╚"  BR_G="╝"
    local HL="═"   VL="║"
    declare -A CORNER_TOP_LEFT=(
        [TL]="$VHL"
        [TR]="$TL_G"
        [BL]="$VHL"
        [BR]="$VHL"
    )
    declare -A CORNER_TOP_RIGHT=(
        [TL]="$TR_G"
        [TR]="$VHR"
        [BL]="$VHR"
        [BR]="$VHR"
    )
    declare -A CORNER_BOTTOM_LEFT=(
        [TL]="$VHL"
        [TR]="$TL_G"
        [BL]="$BL_G"
        [BR]="$BL_G"
    )
    declare -A CORNER_BOTTOM_RIGHT=(
        [TL]="$VHL"
        [TR]="$TL_G"
        [BL]="$BL_G"
        [BR]="$BL_G"
    )

    BLOCK=()
    
    # Top border
    # BLOCK+=("$(printf "%s%s%s" "$VHL" "$(printf "%*s" "$((HALF_WIDTH+2))" "" | sed "s/ /$HL/g")" "$VHR")")
    BLOCK+=("$(printf "%s%s%s" "${CORNER_TOP_LEFT[$type]}" "$(printf "%*s" "$((HALF_WIDTH+2))" "" | sed "s/ /$HL/g")" "${CORNER_TOP_RIGHT[$type]}")")

    # Title
    BLOCK+=("$(printf "%s %-*s %s" "$VL" "$HALF_WIDTH" "$title" "$VL")")

    # Spacer line
    # BLOCK+=("$(printf "%s %-*s %s" "$VL" "$HALF_WIDTH" "" "$VL")")

    # Listing lines
    while IFS= read -r line; do
        BLOCK+=("$(printf "%s %-*s %s" "$VL" "$HALF_WIDTH" "$line" "$VL")")
    done <<< "$list_input"

    # Bottom border
    # BLOCK+=("$(printf "%s%s%s" "${CORNER_BOTTOM_LEFT[$type]}" "$(printf "%*s" "$((HALF_WIDTH+2))" "" | sed "s/ /$HL/g")" "${CORNER_BOTTOM_RIGHT[$type]}")")
}

sparkline() {
    local values=("$@")
    local min max range scaled char
    local chars=(▁ ▂ ▃ ▄ ▅ ▆ ▇ █)

    # determine min/max
    min=${values[0]}
    max=${values[0]}
    for v in "${values[@]}"; do
        (( $(echo "$v < $min" | bc -l) )) && min=$v
        (( $(echo "$v > $max" | bc -l) )) && max=$v
    done

    range=$(echo "$max - $min" | bc -l)

    for v in "${values[@]}"; do
        scaled=$(echo "(($v - $min) / $range) * 7" | bc -l)
        idx=$(printf "%.0f" "$scaled")
        char=${chars[$idx]}
        printf "%s" "$char"
    done
    echo
}



while true; do
    clear
    # Draw top border
    printf "%s%s%s\n" "$TL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$TR"
    printf "%s%*s%s%*s%s\n" "$VL" "$PAD" "" "$TITLE" "$((WIDTH - PAD - ${#TITLE}))" "" "$VL"
    printf "%s%s%s\n" "$VHL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$VHR"

    printf "%s %-*s %s\n" "$VL" "$((WIDTH-2))" "Tail of $LOGFILE" "$VL"
    LOG_OUT=$(tail -n 15 "$LOGFILE" | sed 's/.*\r//')
    # Body lines
    echo "$LOG_OUT" | while IFS= read -r line; do
        printf "%s%-*s%s\n" "$VL" "$WIDTH" "$line" "$VL"
    done

    OUT=$(find data -type f -newer "$LOGFILE" -ls | grep "mod_" | wc -l)
    OUT2=$(find data -newer "$LOGFILE" -ls | grep "csv" | wc -l)
    NEW_FILES=$(find data -type f -newer "$LOGFILE" -name "mod_*" -mmin -10 | wc -l)
    RATE=$(echo "scale=2; $NEW_FILES / 10" | bc -l) 
    NEW_FILES1=$(find data -type f -newer "$LOGFILE" -name "mod_*" -mmin -2 | wc -l)

    # sparkline "${rates[@]}"

    printf "%s%s%s\n" "$VHL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$VHR"
    printf "%s %-*s %s\n" "$VL" "$((WIDTH-2))" "Number of new models: $OUT (complete); $((OUT2 /3 - $OUT)) (running); $(($OUT2)) (Total csv); $RATE (Av. per min in last 10 mins), $NEW_FILES1 (last minute)" "$VL"

    # ────────────────────────────────────────────────
    # Build LEFT block
    # ────────────────────────────────────────────────
    LEFT_INPUT=$(ls -lath "$MOD_PATH"  | grep mod_ | cut -d ' ' -f 6- | head -n 16)
    build_block "Model listing: $MOD_PATH mod_" "$LEFT_INPUT" LEFT_BLOCK "TL"

    # ────────────────────────────────────────────────
    # Build RIGHT block
    # ────────────────────────────────────────────────
    # ref=$(stat -c %y "$(ls -t $MOD_PATH | head -1)" | cut -d. -f1))
    newest="$(ls -1t -- "$MOD_PATH" | grep "gcrmn_model*" | head -n1)"
    ref=$(ls -lt --time-style="+%b %d %H:%M" "$MOD_PATH/$newest" | cut -d ' ' -f 6,7,8)
    RIGHT_INPUT=$(ls -lat --time-style="+%b %d %H:%M" "$MOD_PATH" | grep "gcrmn_model*" | cut -d ' ' -f 6- | grep "$ref" | head -n 16 | sort -k 5 -h -r )
    build_block "Model listing: $MOD_PATH gcrmn_model_" "$RIGHT_INPUT" RIGHT_BLOCK "TR"

    # ────────────────────────────────────────────────
    # Print blocks side-by-side
    # ────────────────────────────────────────────────
    max_lines=$(( ${#LEFT_BLOCK[@]} > ${#RIGHT_BLOCK[@]} ? ${#LEFT_BLOCK[@]} : ${#RIGHT_BLOCK[@]} ))
    PAD_LINE="$(printf "%s %-*s %s" "$VL" "$HALF_WIDTH" "" "$VL")"

    # Pad LEFT_BLOCK
    while ((${#LEFT_BLOCK[@]} < max_lines)); do
        LEFT_BLOCK+=("$PAD_LINE")
    done

    # Pad RIGHT_BLOCK
    while ((${#RIGHT_BLOCK[@]} < max_lines)); do
        RIGHT_BLOCK+=("$PAD_LINE")
    done
    
    for ((i=0; i<max_lines; i++)); do
        printf "%-*s%s%s\n" \
               "$((HALF_WIDTH + 0))" \
               "${LEFT_BLOCK[i]}" \
               "$SPACER" \
               "${RIGHT_BLOCK[i]}"
    done
    
    # ────────────────────────────────────────────────
    # Build LEFT block
    # ────────────────────────────────────────────────
    LEFT_INPUT=$(ls -lath "$MOD_PATH"  | grep -E "posteriors.*|cellmeans.*" | cut -d ' ' -f 6- | head)
    build_block "Posteriors and cellmeans listings: $MOD_PATH posteriors.*|cellmeans.*" "$LEFT_INPUT" LEFT_BLOCK "BL"

    # ────────────────────────────────────────────────
    # Build RIGHT block
    # ────────────────────────────────────────────────
    RIGHT_INPUT=$(ls -lath "$FIG_PATH"  | cut -d ' ' -f 6- | head)
    build_block "Figure listing: $MOD_PATH mod_" "$RIGHT_INPUT" RIGHT_BLOCK "BR"

    # ────────────────────────────────────────────────
    # Print blocks side-by-side
    # ────────────────────────────────────────────────
    max_lines=$(( ${#LEFT_BLOCK[@]} > ${#RIGHT_BLOCK[@]} ? ${#LEFT_BLOCK[@]} : ${#RIGHT_BLOCK[@]} ))
    PAD_LINE="$(printf "%s %-*s %s" "$VL" "$HALF_WIDTH" "" "$VL")"

    # Pad LEFT_BLOCK
    while ((${#LEFT_BLOCK[@]} < max_lines)); do
        LEFT_BLOCK+=("$PAD_LINE")
    done

    # Pad RIGHT_BLOCK
    while ((${#RIGHT_BLOCK[@]} < max_lines)); do
        RIGHT_BLOCK+=("$PAD_LINE")
    done

    for ((i=0; i<max_lines; i++)); do
        printf "%-*s%s%s\n" \
               "$((HALF_WIDTH + 0))" \
               "${LEFT_BLOCK[i]}" \
               "$SPACER" \
               "${RIGHT_BLOCK[i]}"
    done
    
    printf "%s%s%s\n" "$VHL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$VHR"
    OUT="(Updated: $(date))"
    echo "$OUT" | while IFS= read -r line; do
        # printf "%s %-${WIDTH}s %s\n" "$VL" "$line" "$VL"
        printf "%s%-*s%s\n" "$VL" "$WIDTH" "$line" "$VL"
    done
    printf "%s%s%s\n" "$BL" "$(printf "%*s" "$WIDTH" "" | sed "s/ /$HL/g")" "$BR"

    sleep "$REFRESH"
done
