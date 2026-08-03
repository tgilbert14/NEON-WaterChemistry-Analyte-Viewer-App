#!/usr/bin/env bash

# Cold-start-aware semantic production health. Posit host error pages can return
# HTTP 200, so the Water-specific readiness marker is mandatory for the app.

set -uo pipefail

MAX_ATTEMPTS="${SMOKE_MAX_ATTEMPTS:-10}"
SLEEP_BASE="${SMOKE_SLEEP_BASE:-5}"
CONNECT_TIMEOUT="${SMOKE_CONNECT_TIMEOUT:-15}"
MAX_TIME="${SMOKE_MAX_TIME:-45}"
APP_MARKER="${SMOKE_APP_MARKER:-water-chemistry-v1}"
APP_RECEIPT="${SMOKE_APP_RECEIPT:-}"
failed=0

check_one() {
  local label="$1" url="$2" body attempt code nap
  body=$(mktemp)
  for ((attempt = 1; attempt <= MAX_ATTEMPTS; attempt++)); do
    code=$(curl -sS -o "$body" -w '%{http_code}' -L \
      --connect-timeout "$CONNECT_TIMEOUT" --max-time "$MAX_TIME" \
      -A 'ddl-water-chemistry-semantic-smoke/1.0' "$url" 2>/dev/null || echo "000")
    if [[ "$code" =~ ^(2|3)[0-9][0-9]$ ]]; then
      if grep -Eqi 'startup error|application failed to start|application error|service unavailable' "$body"; then
        echo "wait [$label] HTTP $code but host error page detected ($attempt/$MAX_ATTEMPTS)"
      elif [[ "$label" == *"app"* ]] && ! grep -Fq "$APP_MARKER" "$body"; then
        echo "wait [$label] HTTP $code but app marker is absent ($attempt/$MAX_ATTEMPTS)"
      elif [[ "$label" == *"app"* && -n "$APP_RECEIPT" ]] && ! grep -Fq "$APP_RECEIPT" "$body"; then
        echo "wait [$label] HTTP $code but exact runtime receipt is absent ($attempt/$MAX_ATTEMPTS)"
      else
        echo "ok [$label] HTTP $code + semantic body check (attempt $attempt)"
        rm -f "$body"
        return 0
      fi
    else
      echo "wait [$label] HTTP $code ($attempt/$MAX_ATTEMPTS)"
    fi
    if (( attempt < MAX_ATTEMPTS )); then
      nap=$((SLEEP_BASE * attempt)); ((nap > 40)) && nap=40
      sleep "$nap"
    fi
  done
  rm -f "$body"
  echo "DOWN [$label] semantic health not reached"
  return 1
}

if [[ $# -eq 0 ]]; then
  echo "usage: $0 '<label>=<url>' ..." >&2
  exit 2
fi

for spec in "$@"; do
  label="${spec%%=*}"
  url="${spec#*=}"
  if ! check_one "$label" "$url"; then failed=1; fi
done

if [[ "$failed" -ne 0 ]]; then
  echo "post-deploy semantic smoke FAILED" >&2
  exit 1
fi
echo "post-deploy semantic smoke PASSED"
