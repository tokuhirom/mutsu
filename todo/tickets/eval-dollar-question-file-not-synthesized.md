# `EVAL` doesn't synthesize an `EVAL_N` filename for `$?FILE`, and ignores the `:filename` arg

Found by the doc-diff harness (`docs/doc-diff-backlog.md`,
`Type/independent-routines.rakudoc:110`).

## Repro

```raku
use MONKEY-SEE-NO-EVAL;
EVAL 'say $?FILE';                              # raku: a synthetic .../EVAL_0 path
EVAL 'say $?FILE', filename => '/my-eval-code'; # raku: /my-eval-code
```

- `raku`: prints a synthetic `EVAL_0`-suffixed path for the first call (the doc says
  `/tmp/EVAL_0`; current raku actually uses the CWD instead of `/tmp` — that specific
  detail is `raku-drift`, not a mutsu bug), then `/my-eval-code` for the second call
  (honoring the explicit `:filename` argument).
- `mutsu` (`target/debug/mutsu`): both calls print the **outer script's own** file
  path (e.g. `tmp/ddh/prog-....raku` under the harness, or `-e` for `-e` scripts) —
  `$?FILE` inside `EVAL`-compiled code is never given a synthetic name, and the
  `:filename` named argument is silently ignored entirely (both calls print the exact
  same thing).

## Minimal isolation

```raku
use MONKEY-SEE-NO-EVAL;
EVAL q[say $?FILE];
EVAL q[say $?FILE], filename => "/my-eval-code";
```

- `raku`: two different lines — a synthetic `EVAL_0` path, then `/my-eval-code`.
- `mutsu`: `-e` printed twice (the outer script's own file, `:filename` ignored).

## Affected files (starting point)

- Wherever `EVAL`/`EVALFILE` is implemented and compiles/executes a sub-source string
  (grep for `"EVAL"` in `src/runtime/`) — needs to (1) synthesize a per-call
  `EVAL_<N>` filename (an incrementing counter) for the re-entrant compilation's
  `$?FILE`/`$*PROGRAM-NAME`-equivalent context when no `:filename` is given, and (2)
  honor an explicit `filename =>` named argument when present.
