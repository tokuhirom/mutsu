# The battery gate retries an upstream fetch before giving up

`scripts/battery-testsuite.sh` fetches each bundled library's upstream commit
before it runs that library's test suite. `fetch_commit` tried exactly once.
On 2026-09-24 git.sr.ht answered HTTP 502 for a few minutes, which cut off
`Terminal::ANSI`. `test-suites` then printed
`GATE ERROR: a battery test suite could not be set up` on #9269's first run
and again on its re-run, although the PR only touched the regex engine. Any
PR in CI during those minutes would have failed the same way.

The fetch is now retried with a 5s/10s/15s backoff, four attempts by default
(`BATTERY_FETCH_ATTEMPTS`), and each retry prints a warning. The gate is no
weaker: a battery that is still unfetchable after the last attempt fails it
exactly as before.

Closes #9275.
