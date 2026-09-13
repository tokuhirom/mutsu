# A smartmatch stops re-interning its fixed env keys on every match

`"a" ~~ /a/` spent more time interning the *names* of the env slots it touched
than it did running the matcher. Callgrind put `Symbol::intern` at 16.4% of that
call against 8.2% for the regex engine proper — and the interned names were all
fixed literals the process had already seen millions of times.

The cause was the string-keyed `Env` API. `env.get("_")` and
`env.insert("_".to_string(), v)` intern their key on every call; the thread-local
memo makes a repeat intern a hit rather than a table insert, but a hit still
hashes and compares the whole key string, and `insert` additionally allocates the
`String` to hand it. Four places on the per-match path did this:

- `exec_smart_match_expr_op` (the VM opcode every compiled `~~` runs) reads or
  writes `$_` up to five times per match — saving the topic, probing it for an
  aliasing container, installing the subject, reading it back to decide whether a
  destructive RHS modified it, and restoring it.
- The runtime's single-regex arm does its own save/install/restore of `$_`,
  removes and re-reads `made` around the inline-action reduction, and writes `$/`.
- `smart_match_op` reads `$/` back to decide what the operator returns.
- Both capture-application paths write `$0`..`$N` under an `i.to_string()` key —
  a fresh `String` allocation plus an intern, per capture, per match, and twice
  over, since positional captures are written first as strings and then upgraded
  to `Match` objects.

All of them now go through the symbol-keyed `Env` entry points. `_` was already
in `symbol::wk`; `/` and `made` were added next to it, and positional-capture
indices get a small `capture_index(i)` table that interns each index lazily on
first use. Lazily, not eagerly: interning a numeric name registers it in the
symbol table's capture-shape registry, which `reset_capture_env_vars` probes once
per match, so priming all sixteen up front would have charged a program that only
ever uses `$0` fifteen extra probes per match forever.

One unrelated copy went with them. The arm built the topic as
`Value::str(text.clone())` while handing `&text` to the matcher — a full copy of
the subject string on every `~~`, purely to have it in two places. The topic is
now built from the subject directly and the `&str` borrowed back out of it, so
the two share one allocation and the second reference costs a refcount bump.

Measured as interns per `~~`, net of the loop carrying it
(`tests/regex_match_intern_budget.rs`):

| match | before | after |
|---|---:|---:|
| `"a" ~~ /a/` | 16 | 4 |
| `"abc" ~~ /(a)(b)(c)/` | 22 | 4 |
| `"abc" ~~ /$<first>=(a)/` | 18 | 6 |

Measured on top of #8262 and #8263, which landed first and removed most of what
*else* a match interned (a `Match` that is never forced never interns its
attribute names). Against the base those two left, this removes three quarters of
what remains.

The capturing case is the one worth reading twice: it now costs the same as the
capture-free case, where before it cost six more. Captures no longer add
interning at all.

The pin is deterministic rather than timed. `symbol::intern_calls()` is a new
exact per-thread counter of `Symbol::intern` calls — distinct from the existing
`interned_count()`, which counts the names that exist rather than the asks — so
the test measures how much interning one *additional* match performs and asserts
a budget on it, with no dependence on machine speed or load.

What is left on that path is other paths' interning, and is deliberately not
addressed here: the type-object names a smartmatch resolves (`Bool`, `Any`,
`Cool`, `Mu`, `Capture`), the `Match` object's own attribute names (`str`,
`from`, `to`, `list`, `named`, `orig`), and the `<name>` keys of named captures,
which are still built with `format!` per match. The last of those is a different
fix from these fixed literals — the name varies per pattern — and the
named-capture budget in the test documents it rather than asserting it away.

Closes #8269. Part of Stage 0 of ADR-0099.
