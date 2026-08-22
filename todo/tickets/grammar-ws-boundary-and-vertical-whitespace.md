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

## Suggested next step

Isolate further: does a *plain* custom `ws { \h* }` (no `<!ww>`) already work for the `rule TOP {
a b '.' }` case? That would narrow the bug to `<!ww>` specifically rather than custom-`ws`
dispatch in general.
