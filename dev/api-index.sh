#!/usr/bin/env bash
# Public API index of express-cljs from the sources (namespace, var, first
# docstring line).
#   dev/api-index.sh              tab-separated: ns/var <TAB> first doc line
#   dev/api-index.sh --org        org-mode description lists, one per namespace
#                                 (pasted into docs/express-cljs.org "API reference")
#   dev/api-index.sh --check      verifies that every public var is mentioned in
#                                 docs/express-cljs.org as =ns/var= and that every
#                                 public var has a docstring; exit 1 otherwise.
set -euo pipefail
cd "$(dirname "$0")/.."

index() {
  for f in src/express_cljs/*.cljs; do
    ns=$(head -1 "$f" | sed -E 's/^\(ns ([^ ]+).*/\1/')
    awk -v ns="$ns" '
      function firstline(s) { gsub(/\\"/, "\001", s); gsub(/^[ \t]*"/, "", s); sub(/".*$/, "", s); gsub(/\001/, "\"", s); return s }
      /^\(def(n|macro)? / && $0 !~ /\^:private/ {
        name=$2; line=$0
        sub(/^\(def(n|macro)? [^ ]+ */, "", line)
        if (line ~ /^"/) { doc=firstline(line) }
        else { getline nxt; if (nxt ~ /^[ \t]*"/) doc=firstline(nxt); else doc="(NO DOCSTRING)" }
        printf "%s/%s\t%s\n", ns, name, doc
      }' "$f"
  done
}

case "${1:-}" in
  --org)
    current=""
    while IFS=$'\t' read -r var doc; do
      ns="${var%%/*}"
      if [ "$ns" != "$current" ]; then current="$ns"; printf "\n*** =%s=\n" "$ns"; fi
      printf -- "- =%s= :: %s\n" "$var" "$doc"
    done < <(index)
    ;;
  --check)
    manual="${2:-docs/express-cljs.org}"
    rc=0
    while IFS=$'\t' read -r var doc; do
      if [ "$doc" = "(NO DOCSTRING)" ]; then echo "no docstring: $var"; rc=1; fi
      if ! grep -q -- "=$var=" "$manual"; then echo "missing from $manual: $var"; rc=1; fi
    done < <(index)
    [ "$rc" = 0 ] && echo "api-index: $(index | wc -l) public vars, all documented and referenced in $manual"
    exit $rc
    ;;
  *) index ;;
esac
