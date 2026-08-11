# http-session-inmemory/persistent crash with rc=139 (SIGSEGV) at test 2 on main

Observed 2026-08-11 on main (`6985fdb14`, debug build) while verifying
ADR-0025 slice 1 — reproduced with the spike REVERTED, so it is a
pre-existing main regression, not part of the capture-cell campaign:

```
bash -c 'INC=$(cat tmp/cro-work/inc-paths.txt); \
  CRO=tmp/cro-work/C_RO_CRO_HTTP_3a9832b52924f07d3c66fadcd2309ab8e0cffa41; \
  timeout 180 target/debug/mutsu $INC $CRO/t/http-session-inmemory.rakutest'
# -> rc=139, ok=2, no "not ok"
```

Both `http-session-inmemory.rakutest` and `http-session-persistent.rakutest`
die the same way after 2 passing tests. On 2026-08-09 (release sweep,
session 84 diagnosis data) these files ran to 10/13 and notok=4
respectively — so something merged between 2026-08-09 and 2026-08-11
turned a partial pass into a segfault. Candidates in that window include
the ADR-0022 slice 1 (LTM), ADR-0024 (mainline lexical cells) + its two
CI-regression fixes, and the #6132-#6217 range.

Suggested attack: bisect main over that window against the inmemory file
(debug build, the exact command above), then rust-gdb the segfault
(`rust-gdb -batch -ex run -ex bt --args target/debug/mutsu $INC ...`).

Blocks: the session-expiry acceptance criterion of
`todo/deep/closure-read-only-capture-loses-to-caller-env-same-name.md`
(ADR-0025) — the staleness fix itself is pinned by
`t/closure-capture-instance-cell.t` tests 3-4 and does not depend on this.
