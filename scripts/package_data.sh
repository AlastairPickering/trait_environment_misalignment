#!/usr/bin/env bash
set -euo pipefail

TAG=data-v1
SPLIT_MB=1900
OUT="rel/$TAG"

cd "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
mkdir -p "$OUT"

EXCLUDES=( "**/.DS_Store" "**/__pycache__/*" ".venv/*" "**/.smbdelete*" )

zip -r "$OUT/data_climate_v1.zip"        data/climate        -x "${EXCLUDES[@]}"
zip -r "$OUT/data_forest_plots_v1.zip"   data/forest_plots   -x "${EXCLUDES[@]}"
zip -r "$OUT/data_metadata_v1.zip"       data/metadata       -x "${EXCLUDES[@]}"
zip -r "$OUT/data_precomputed_v1.zip"    data/precomputed    -x "${EXCLUDES[@]}"
zip -r "$OUT/data_traits_v1.zip"         data/traits         -x "${EXCLUDES[@]}"

cd "$OUT"
for z in *.zip; do
  [ -e "$z" ] || continue
  bytes=$(stat -f%z "$z")
  if [ "$bytes" -ge $((SPLIT_MB*1024*1024)) ]; then
    echo "Splitting $z …"
    zip -s ${SPLIT_MB}m --out "split_$z" "$z"
    rm "$z"
  fi
done

shasum -a 256 * > SHA256SUMS.txt 2>/dev/null || true
echo "[done] Upload everything in: $(pwd)"
