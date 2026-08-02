# A regex literal captures its defining scope

A Raku regex is a closure over the scope it was written in. mutsu represented a
regex value as a bare pattern string (`ValueRepr::Regex(Arc<String>)`, or
`RegexWithAdverbs` when the literal carried adverbs), so code embedded in the
pattern — `{ ... }` blocks, `<?{ ... }>` assertions, `:my`/`:let` initializers —
resolved its free variables against whatever env existed at *match* time. A
regex built in one frame and matched from another silently lost them.

```raku
sub make-rx() {
    my @h = "aa", "bb";
    / ^ 'x' <?{ @h[1] eq 'bb' }> $ /
}
"x" ~~ make-rx();   # was: no match (the assertion read an empty @h)
```

## What changed

A code-bearing regex literal now loads through a new
`OpCode::LoadRegexClosure` instead of a plain `LoadConst`. The compiler scans
the pattern for the sigiled names its embedded code can reference (reusing the
scanner the closure free-variable analysis already had) and bakes them into the
op, together with the creating frame's local slot for each name. At load time
the VM reads those names out of the frame — local slot first, then `env` — and
attaches the snapshot to the value. A name that resolves nowhere is left out,
so a literal that captures nothing is byte-identical to the old constant.

The captured scope rides in a new `Value::RegexCaptured` repr variant
(`RegexClosure { pattern, scope }`) whose `view()` deliberately yields
`ValueView::Regex(&pattern)`. That is what makes the change tractable: ~120
sites match `ValueView::Regex(_)` specifically — grammar/token registration
among them — and every one of them keeps working verbatim, seeing a plain
pattern. Only the match entry points probe the repr, through
`Value::regex_closure_scope()`. An earlier attempt that instead promoted the
value to `RegexWithAdverbs` broke exactly those sites (a grammar token stopped
running its code block at all) and had to be reverted; adverb-bearing literals
are covered here too, via a new `RegexAdverbs::captured` field.

`smart_match` (`~~`, `given`/`when`) and the `.match` / `.subst` methods
install the captured scope into `env` for the *whole* match and restore it
afterwards. Installing once around the match, rather than at each of the
half-dozen places the engine evaluates embedded code, means the inline
`{ ... }` blocks, the `<?{ ... }>` assertions, the `:my`/`:let` initializers
and the reduce-time `make` replay all see it — while the regex's own `:my`
lexicals and the capture variables still win, because the engine installs those
on top.

Uninstalling skips any binding the embedded code *rebound*: a regex code block
assigning to a lexical is a closure write, and restoring the shadow over it
would lose `/ a { $n = 42 } /`'s effect (`t/regex-code-block-writeback.t`).
The check is by binding identity (`Value::same_binding`), the same test the
existing writeback bookkeeping uses.

`Value::gc_trace` gained a repr-level probe for the captured scope. Because the
value views as a plain Regex, the `ValueView` match arms cannot reach the
`Value`s it owns, and a cycle routed through a captured lexical would otherwise
be an invisible edge to the cycle collector.

## Why it mattered

This was the last blocker for `Cro::HTTP` serving a request whose route has
captured segments. `Cro::HTTP::Router::RouteSet!generate-route-matcher` builds
its path matcher with `EVAL 'regex { ... }'` over a local `my @handlers`, and
`transformer` matches it much later from a `supply` block; the per-route bind
check `<?{ my $han = @handlers[$i]; $han.signature.ACCEPTS($cap) ... }>` read an
empty array, so the first parameterised route aborted the run with
`No such method 'signature' for invocant of type 'Any'`.
`t/http-router.rakutest` went from 19 passing subtests (then an abort) to 31 of
51 with the file running to completion.

Pinned by `t/regex-literal-is-a-closure.t` (9 assertions) and by the
previously-`todo` assertion 4 of `t/regex-my-initializer-and-escaping-sub.t`.

## Known limits

- The capture is a *snapshot* of the values, so a later **rebinding** in the
  defining scope is invisible (`my @h; my $rx = / a <?{ @h }> /; @h.push(1)`).
  A capture that is already a shared `ContainerRef` cell is kept as the cell,
  so mutations through a boxed lexical do track; making every capture a cell,
  the way closures do, is the sound end state.
- Only `~~`, `.match` and `.subst` install the scope. Other entry points
  (`.comb`, `.split`, `Grammar.parse` with a regex value) fall back to today's
  match-time resolution — no regression, just no capture.
- A write from embedded code to a *captured* lexical lands in the match-site
  env rather than travelling back to the defining frame.
