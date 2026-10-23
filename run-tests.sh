#!/usr/bin/env bash
# Compiles the cljs.test suite (shadow-cljs :node-test build) and runs it, in
# two lanes: the development build, then a release build at :advanced.
# Exit status: 0 when every test of both lanes passes, non-zero otherwise.
#
# Why two lanes: Closure's advanced optimizations rewrite function
# signatures (unused trailing parameters are dropped), and Express reads
# fn.length to tell an error handler from a request handler — a wrapper that is
# correct in the development build can lose its error layers in the code a
# consumer actually ships. The suite therefore also runs on the :advanced
# output; the development lane runs first because its failures are readable.
#
# The build has :autorun true (tests execute on every compile/watch), but
# shadow-cljs does not propagate the autorun exit status, so we disable autorun
# for these compiles and run the generated scripts ourselves.
#
# CAVEAT: the last line is `exec` — this script REPLACES its shell with node, so
# never chain it mid-command (`./run-tests.sh && something` is fine from a parent
# shell, but nothing placed after the exec line inside this file would run).
#
# The tests never touch the network beyond the loopback interface: `npm install`
# (first run only) fetches the toolchain plus `express`/`ws`; server tests
# listen on an ephemeral 127.0.0.1 port and close it before the process exits.
#
# The publicity gate runs FIRST: src/ and README.md are public documentation.
set -euo pipefail
cd "$(dirname "$0")"

if [ ! -d node_modules ]; then
  npm install
fi

bash test/publicity_gate.sh

# Lane 1: the development build (what `npx shadow-cljs compile` produces).
npx shadow-cljs compile node-test --config-merge '{:autorun false}'
node target/node-test.js

# Lane 2: the :advanced release build, to its own output file so lane 1's
# output stays on disk.
npx shadow-cljs release node-test --config-merge '{:autorun false :output-to "target/node-test-advanced.js" :compiler-options {:optimizations :advanced}}'
exec node target/node-test-advanced.js
