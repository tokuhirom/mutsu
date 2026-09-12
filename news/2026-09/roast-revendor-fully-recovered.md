# The 2026-09-11 roast re-vendor is fully paid off — whitelist at an all-time 1437

The [roast re-vendor](roast-revendor-2026-09.md) to upstream `85a87909` cost five whitelist entries
on the day it landed, because upstream had added subtests for five still-open rakudo bugs plus a
brand-new 586-subtest file. Each gap was filed as its own issue with a repro and a measured rakudo
comparison. **All seven files are now whitelisted**, every issue closed by a merged fix:

| Issue | Fix | File | Recovered |
| --- | --- | --- | --- |
| #7902 | `8fdafe81` C99 hexadecimal float literals | `S02-literals/numeric.t` | 89 subtests |
| #7903 | `4f970ce7` `sprintf` `%a` / `%A` | `S32-str/sprintf-a.t` (new) | 586 subtests |
| #7904 | #7935 numify Match captures in `sum` | `S05-capture/caps.t` | 56 subtests |
| #7905 | #7938 preserve subtraction in a negated class lookahead | `S05-metasyntax/charset.t` | 90 subtests |
| #7906 | #7945 preserve block-quantifier bounds on backtracking | `S05-metasyntax/regex.t` | 68 subtests |
| #7907 | #7962 preserve expanded ignorecase class entries | `S05-modifier/ignorecase.t` | 115 subtests |

The whitelist is **1437 / 1465** — 28 files not whitelisted, the fewest it has ever been, and two
files *better* than the 1435 it stood at before the re-vendor. The net gain is the new
`sprintf-a.t` (586 subtests) and `numeric.t`'s recovery.

## The two that were left behind

`caps.t` and `regex.t` were the reason this entry exists. Their fixes (#7935 and #7945) merged on
2026-09-11, and both files pass completely on `main` — but neither was put back into
`roast-whitelist.txt`, so for a day CI was not guarding 124 subtests that had already been made to
work. `TODO_roast/BLOCKERS.md` had drifted the same way: it still carried all four regex rows and a
stale `1433 / 1465` count against an actual 1435.

Re-measured on `dcc038dc` (release build, `MUTSU_FUDGE=1 prove`), all four files pass:

```
PASS  roast/S05-capture/caps.t             (was NOT whitelisted)
PASS  roast/S05-metasyntax/regex.t         (was NOT whitelisted)
PASS  roast/S05-metasyntax/charset.t       (whitelisted)
PASS  roast/S05-modifier/ignorecase.t      (whitelisted)
```

so both were added and the now-empty "Files dewhitelisted by the 2026-09-11 roast re-vendor"
section was deleted from `BLOCKERS.md` with the counts refreshed.

**The lesson is the gap itself, not the two files.** Closing the issue and merging the fix is not
the end of a dewhitelisted roast file: the whitelist entry is a separate artifact, and nothing in
CI notices that a file which passes is absent from it — the suite only runs what the whitelist
names, so a recovered file simply stays silently unguarded. When a fix closes a roast blocker,
re-running the file and restoring its whitelist line is part of that fix, and `docs/vendoring.md`'s
post-re-vendor checklist is where that expectation lives.

## Where mutsu now stands against the oracle

All six of these files were spec tests written against rakudo bugs that are *still open upstream*,
so Rakudo v2026.07 fails every one of them. Having implemented all six, mutsu now scores strictly
higher than the local rakudo on each:

| File | mutsu | raku v2026.07 |
| --- | --- | --- |
| `S05-capture/caps.t` | 56/56 | 43/56 |
| `S05-metasyntax/charset.t` | 90/90 | 57/90 |
| `S05-metasyntax/regex.t` | 68/68 | 58/68 |
| `S05-modifier/ignorecase.t` | 115/115 | 103/115 |
| `S02-literals/numeric.t` | 89/89 | SORRY |
| `S32-str/sprintf-a.t` | 586/586 | 0/586 |

That is the case for keeping the vendored roast current, in one table: the update did not cost four
files, it named six concrete, well-scoped features worth implementing and they were all done inside
a day.
