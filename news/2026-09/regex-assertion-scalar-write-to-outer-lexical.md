# A regex assertion's write to an outer scalar lexical now reaches the caller

An embedded `<?{ … }>` / `<!{ … }>` assertion that assigned to a scalar lexical
declared outside the regex left the caller's variable untouched:

```raku
my $n = 0;
"aaaa" ~~ / [ <?{ $n++; True }> . ]+ /;
say $n;        # raku: 5      mutsu (before): 0
```

A plain `{ … }` code block in the same position wrote back correctly, and so did
a *mutation* of a container from inside an assertion (`@a.push(1)` — the caller's
slot already shares that allocation). Only a scalar rebinding was lost, and it
died with the match.

## Why it was lost

`eval_regex_inline_code` takes a `writes_back_to_caller` flag. A plain `{ … }`
block passes `true` and runs through `eval_regex_code_block_body`, which
snapshots the env, diffs it by binding identity afterwards, and logs every
rebound name into `pending_local_updates` so the VM refreshes the caller's
compiled local slot. An assertion passed `false`: ADR-0009 deliberately kept
that path snapshot-free, because an assertion is evaluated at *every cursor
position* and a full env snapshot plus identity diff per position is exactly the
cost that path exists to avoid.

Leaving the write in `env` was enough for a later `$name` interpolation or
assertion inside the same match to read it back, and enough for a mutated
container, but nothing ever refreshed the caller's slot — so the value
disappeared as soon as the match ended.

## The fix

The compiler already knows precisely which free variables a block assigns:
`CompiledCode::free_var_writes`, the same set `eval_block_value_recording_writes`
uses for a `where` clause. That set is exactly what the env diff would have
found, and it is computed at compile time, so an assertion that assigns nothing
reports nothing and pays nothing — the hot path keeps ADR-0009's property.

`eval_block_value_cached_reporting_writes` runs the cached chunk and hands the
names back to the caller, and the regex layer owns the filtering policy it needs:
the regex's own `:my` / `:let` lexicals, `$/` / `$¢` / `$0`…, the body's own `my`
declarations and the `make` slot are all regex-scoped and must not reach a caller
slot, and a body compiled inside `grammar G { … }` records the write under the
auto-package-qualified `G::x` while `in_regex_code_block` redirects the write
itself onto the bare lexical, so the package prefix is stripped before the two
names are compared. Names already logged are skipped, which keeps the log bounded
by the number of distinct variables rather than by the number of cursor positions
the assertion ran at.

## Pins

`t/regex-assertion-outer-scalar-write.t` covers the positive and negative
assertion forms, a plain assignment, the two shapes that already worked, a write
that survives a failing match, an assertion's own `my` staying lexical, a
routine-local slot, and a grammar token's assertion. Every assertion in it was
checked against rakudo.

`t/regex-inline-code-compile-cache.t` had routed its counters through `@` / `%`
containers specifically to dodge this bug; it now pins the scalar forms too, so
serving a cached compile cannot lose the per-evaluation writeback either.

Closes [#7593](https://github.com/tokuhirom/mutsu/issues/7593).
