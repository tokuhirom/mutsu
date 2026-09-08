# A regex assertion's write to an outer scalar lexical never reaches the caller

An embedded `<?{ … }>` / `<!{ … }>` assertion that assigns to a scalar lexical
declared outside the regex leaves the caller's variable untouched. A plain
`{ … }` code block in the same position writes back correctly, and so does a
*mutation* of a container from inside an assertion — only the scalar rebinding
is lost.

## Repro

```raku
my $n = 0;
"aaaa" ~~ / [ <?{ $n++; True }> . ]+ /;
say $n;        # raku: 5      mutsu: 0

my @a;
"aaaa" ~~ / [ <?{ @a.push(1); True }> . ]+ /;
say @a.elems;  # raku: 5      mutsu: 5   <- container mutation is fine

my $c = 0;
"aaaa" ~~ / [ { $c++ } . ]+ /;
say $c;        # raku: 5      mutsu: 5   <- a plain `{ }` block is fine
```

## Root cause

`eval_regex_inline_code` (`src/runtime/regex/regex_eval.rs`) takes a
`writes_back_to_caller: bool`. A plain `{ … }` block passes `true` and goes
through `eval_regex_code_block_body`, which diffs the env by binding identity and
logs each rebound name into `pending_local_updates` so the VM refreshes the
caller's compiled local slot. An assertion passes `false` — the doc comment
records this as ADR-0009 deliberately keeping "the cheaper behaviour of simply
leaving the write in `env`, so the hot `<?{ … }>` path does not take on a full
env snapshot per cursor position".

Leaving it in `env` is enough for a later `$name` interpolation or assertion
*inside the same match* to read it, and it is enough for a container (the caller
already shares the allocation), but a scalar rebinding never reaches the caller's
slot, so the value dies with the match.

## Why this is not a one-line fix

Flipping assertions to `writes_back_to_caller: true` would make the hot path pay
a full env snapshot and identity diff at **every cursor position** — the exact
cost ADR-0009 avoided, and the same carrier-prologue overhead measured in
`todo/perf/regex-inline-code-carrier-prologue-overhead.md`. The right shape is
almost certainly to use the compiled body's `free_var_writes` (the compiler
already knows exactly which free variables the block assigns, and
`eval_block_value_recording_writes` already exploits that for `where` clauses)
instead of an env diff, so an assertion that writes nothing pays nothing.

## Pin when fixed

`t/regex-inline-code-compile-cache.t` currently routes its counters through a
`@`/`%` container specifically to dodge this; three assertions of an earlier
draft failed only because of it. Add the scalar forms above when it is fixed.
