#!/usr/bin/env bash
# Run z3 on every rule file produced by `gen_vopt -compiler-smt-dir DIR`
# and summarise: unsat = verified, sat = counterexample, unknown = timeout.
# Rules whose assumptions are unsatisfiable are reported as VACUOUS.
# Usage: ./check_rules.sh DIR [timeout-seconds] [parallel-jobs] [memory-MB-per-job]
# z3 stops a job that exceeds its memory limit (reported as unknown/error).
set -euo pipefail

dir=${1:?usage: $0 DIR [timeout-seconds] [parallel-jobs] [memory-MB-per-job]}
timeout=${2:-60}
jobs=${3:-1}
memory=${4:-4000}

check() {
  # buffer the report so that parallel jobs do not interleave their lines
  printf '%s' "$(check_one "$@")"$'\n'
}

check_one() {
  local out name vacuity result
  out=$(z3 -T:"$2" -memory:"$3" "$1" 2>&1 || true)
  name=$(sed -n 's/^verifying //p' <<<"$out")
  # first answer: are the assumptions satisfiable; second: the rule itself
  vacuity=$(grep -E '^(sat|unsat|unknown|timeout)$' <<<"$out" | sed -n 1p)
  result=$(grep -E '^(sat|unsat|unknown|timeout)$' <<<"$out" | sed -n 2p)
  if [[ $vacuity == unsat ]]; then
    printf '  VACUOUS   %s\n' "$name"
  elif [[ $result == unsat ]]; then
    printf '  ok        %s\n' "$name"
  elif [[ $result == sat ]]; then
    printf '  FAILED    %s\n' "$name"
    # the counterexample printed by (get-value ...)
    awk 'n == 2 { print "              " $0 } /^(sat|unsat|unknown|timeout)$/ { n++ }' <<<"$out"
  else
    printf '  %-9s %s\n' "${result:-error}" "$name"
    grep -m1 -i 'error\|memory' <<<"$out" | sed 's/^/              /' || true
  fi
}
export -f check check_one

ls "$dir"/*.smt2 | xargs -P "$jobs" -I{} bash -c 'check "$@"' _ {} "$timeout" "$memory" \
  | tee /dev/stderr \
  | awk '/^  ok /{ok++} /^  FAILED /{f++} /^  VACUOUS /{v++} /^  (unknown|timeout|error) /{u++}
         END { printf "verified: %d, counterexamples: %d, vacuous: %d, unknown: %d\n", ok, f, v, u }' >&2
