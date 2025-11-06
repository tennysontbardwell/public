#!/usr/bin/env bash
set -euo pipefail

IFS=" "

declare -r ARGS="$*"

IFS=$'\n\t'

declare -r TMP_DIR="$(mktemp -d)"
declare -r TMP_FILE="${TMP_DIR}/niimbot-img.png"

# -size 300x70 \

magick \
    -gravity center \
    -background white \
    -fill black \
    -size 320x96 \
    -bordercolor white \
    -font "DejaVu-Sans" \
    "caption:${ARGS}" \
    "${TMP_FILE}"
# -border 20x20 \

echo $TMP_FILE
# open "$TMP_FILE"
# exit

kitty +kitten icat "$TMP_FILE"
echo Enter to continue. CTRL-C to abort...
[ "${NOCONFIRM:-0}" == "1" ] || read

# cd "${HOME}/repos/labbots/NiimPrintX"

# poetry run python -m NiimPrintX.cli.command print \
#        --model d110 \
#        --image "${TMP_FILE}" \
#        --rotate 90 \
#        --density 5

cd ${HOME}/repos/MultiMote/niimblue-node


pnpm run cli print \
       --transport ble \
       --address D110_M-H823110432 \
       --print-task D110M_V4 \
       --print-direction left \
       --density 5 \
       "${TMP_FILE}"



# rm "$TMP_FILE"
# rm -d "$TMP_DIR"
