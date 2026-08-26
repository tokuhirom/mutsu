# Backtrace::Frame's .gist and .raku now render Rakudo's attribute shape

`Backtrace::Frame.gist` and `.raku` used to diverge sharply from Rakudo:
`.gist` reused the frame's `.Str` text (`  in block <unit> at f.raku line 3`),
and `.raku` fell through to the generic no-declared-attributes instance
renderer, producing a bare `Backtrace::Frame.new` with nothing inside the
parens.

Rakudo's `Backtrace::Frame` has no custom `.gist` method, so it inherits the
default `Mu` rendering, which is identical to `.raku`:

```
Backtrace::Frame.new(file => "...", line => 3, code => -> { ... }, subname => "<unit>")
```

mutsu now renders the same shape for both methods. `.Str` is unchanged (it
stays the concise `  in block/sub ... at ... line N` text, which is what
`Backtrace.full`/`.concise`/`.summary` are built from).

## Why this needed `&mut self`

A `Backtrace::Frame` instance only carries `subname`/`file`/`line` attributes;
`.code` is synthesized on demand as a `Routine` stub (mutsu retains no real
callframe object to point `.code` at). Rendering `code => ...` therefore means
recursively invoking that synthesized value's own `.raku`, which needs the
interpreter (`&mut self`) — something the pure, `&self`-free fast-path cascade
in `builtins/methods_0arg/mod.rs` cannot do. The new rendering lives in
`default_instance_repr` (`runtime/methods_instance_ops.rs`), the same place
that already special-cases `IO::Path::Parts`, `Stash`, and a handful of other
native pseudo-classes whose `.raku`/`.gist` need to recurse into a field's own
rendering.

`.gist` for `Backtrace::Frame` moved off the fast path entirely (it now falls
through to the same `default_instance_repr` default that `.raku` already used
to reach), so its `native_method_row_table.rs` arity row was removed — it was
never rowed for `.raku` either, since that method was never in the fast-path
cascade to begin with.

A byte-identical `.raku` is not achievable: Rakudo's `code => ...` embeds a
`Block`/`Method`'s per-run memory address, which mutsu's synthesized `Routine`
stub does not model in the same way. The regression test
(`t/backtrace-frame-gist-raku.t`) therefore asserts on the attribute shape
(via a regex), not on an exact string, and passes unmodified under both
`raku` and `mutsu`.

Frame **count** and frame-model differences (mutsu has no Raku-written CORE
setting, so it never has Rakudo's extra `SETTING::` frames) are unrelated and
remain deliberately deferred — see
`todo/tickets/backtrace-has-fewer-frames-than-rakudo.md`.
