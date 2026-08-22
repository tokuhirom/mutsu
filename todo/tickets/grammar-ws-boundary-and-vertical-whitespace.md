# A custom grammar's `ws` token override doesn't match per raku's whitespace-boundary rules

Discovered via the doc-diff harness on `raku-doc/doc/Language/regexes.rakudoc` (around line
2623).

## Repro

```
grammar Demo {
    token ws {
        <!ww>       # only match when not within a word
        \h*         # only match horizontal whitespace
    }
    rule TOP {
        a b '.'
    }
}
say so Demo.parse("ab.");    # False (no ws required between a and b)
say so Demo.parse("a b.");   # True
say so Demo.parse("a\tb .");  # True
say so Demo.parse("a\tb\n.");  # False (\n is vertical whitespace, ws requires only \h)
```

- raku: `False` / `True` / `True` / `False`
- mutsu: `False` / `False` / `False` / `False` — every case requiring the custom `ws` token to
  actually match whitespace between `a` and `b` fails

## Root cause guess

Either the custom `ws` token override isn't being consulted by the implicit whitespace-skipping
that `rule` (as opposed to `token`) inserts between adjacent atoms, or the `<!ww>` word-boundary
assertion inside the custom `ws` itself is misbehaving, causing the whole `ws` to never match.

## Affected files (starting point)

- `src/runtime/regex.rs` / grammar `rule`-vs-`token` implicit whitespace handling — check where a
  user-defined `ws` token is looked up and invoked between atoms in a `rule`
- `<!ww>` word-boundary assertion implementation

## Update (2026-08-22, batch-6 re-run)

Re-verified on current `main`: the repro above no longer just returns `False` for every case — it
now **crashes** on the very first `.parse` call with `No such method 'ww' for invocant of type
'Match'` (i.e. `<!ww>` inside the custom `ws` token dispatches `.ww` as a method call rather than
recognizing it as the builtin word-boundary assertion). Same root cause area, but the failure mode
regressed from "silently wrong match" to "hard crash, aborts the whole program" — re-found
independently via `raku-doc/doc/Language/regexes-best-practices.rakudoc:163`'s `IniFormat`
grammar example (`token ws { <!ww> \h* }`), which crashes identically. This confirms the bug isn't
specific to the original repro's particular grammar/pattern — any custom `ws` token override using
`<!ww>` hits it.

## Suggested next step

Isolate further: does a *plain* custom `ws { \h* }` (no `<!ww>`) already work for the `rule TOP {
a b '.' }` case? That would narrow the bug to `<!ww>` specifically rather than custom-`ws`
dispatch in general.
