#!/usr/bin/env bash

# Cold-boot the exact six files declared in manifest.json, then require the
# application-specific readiness marker from a real local HTTP response.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
stage="${RUNNER_TEMP:-${TMPDIR:-/tmp}}/water-connect-runtime-${GITHUB_RUN_ID:-local}-$$"
log="${stage}.log"
html="${stage}.html"
port="${WATER_CONNECT_SMOKE_PORT:-3939}"
exact_arg=""
if [[ "${1:-}" == "--require-exact-packages" ]]; then
  exact_arg="--require-exact-packages"
elif [[ $# -ne 0 ]]; then
  echo "usage: $0 [--require-exact-packages]" >&2
  exit 2
fi

cleanup() {
  if [[ -n "${app_pid:-}" ]]; then
    kill "$app_pid" 2>/dev/null || true
    wait "$app_pid" 2>/dev/null || true
  fi
  rm -rf "$stage" "$log" "$html"
}
trap cleanup EXIT
mkdir -p "$stage"

while IFS= read -r path; do
  mkdir -p "$stage/$(dirname "$path")"
  cp "$repo_root/$path" "$stage/$path"
done < <(jq -r '.files | keys[]' "$repo_root/manifest.json")
cp "$repo_root/manifest.json" "$stage/manifest.json"

Rscript --vanilla "$repo_root/scripts/verify_connect_runtime.R" \
  "--app-dir=$stage" ${exact_arg:+"$exact_arg"}

mkdir -p "$stage/.home" "$stage/.tmp" "$stage/.cache"
(
  cd "$stage"
  export HOME="$stage/.home"
  export R_USER="$stage/.home"
  export TMPDIR="$stage/.tmp"
  export XDG_CACHE_HOME="$stage/.cache"
  export LANG=C
  export LC_ALL=C
  exec Rscript --vanilla -e \
    "shiny::runApp('.', host='127.0.0.1', port=$port, launch.browser=FALSE)"
) >"$log" 2>&1 &
app_pid=$!

ready=0
for _attempt in $(seq 1 45); do
  if ! kill -0 "$app_pid" 2>/dev/null; then
    cat "$log"
    exit 1
  fi
  if curl --fail --silent --show-error "http://127.0.0.1:$port/" --output "$html"; then
    ready=1
    break
  fi
  sleep 1
done
if [[ "$ready" -ne 1 ]]; then
  cat "$log"
  echo "Water Connect runtime did not become ready." >&2
  exit 1
fi
kill -0 "$app_pid"
grep -Fq 'water-chemistry-v1' "$html"
expected_receipt=$(jq -r '[.files | to_entries | sort_by(.key)[] | .value.checksum] | join(".")' \
  "$repo_root/manifest.json")
grep -Fq "$expected_receipt" "$html"
echo "Connect six-file HTTP cold boot passed."
