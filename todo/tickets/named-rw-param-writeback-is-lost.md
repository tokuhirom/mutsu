# A named `is rw` parameter never writes back to the caller's variable

```raku
sub g(:$x! is rw) { $x = 7 }
my $v = 1;
g(x => $v);
say $v;   # raku: 7 — mutsu: 1
```

Raku passes the caller's container for a named argument too, so a named
`is rw` parameter aliases `$v` exactly like a positional one. In mutsu the
named-parameter arm of `bind_function_args_values`
(`src/runtime/types/binding_signature.rs`, the `pd.named` loop) has no
`is rw`/`is raw` handling at all: it never pushes an `rw_bindings` entry and
never cell-binds, so the body's write stays in the callee env. The only
rw-adjacent machinery there is the slice-2d container share for `@`/`%`
sources of readonly scalar named params.

Found 2026-08-06 while landing shared-cell rw binding for positional scalars
(`news/2026-08/rw-params-bind-shared-cells.md` — verified this gap predates
that change: the named arm was untouched). The fix should reuse the same
shared-cell path: resolve the caller source name for the named argument (the
`positional_arg_source_name` encoding already carries `key=source` for named
args), then bind the param to a `ContainerRef` cell installed under the
source name, exactly as the positional arm does, and register the
`rw_bindings` entry so the call-site slot resync fires.

Also check `X::Parameter::RW` enforcement: raku rejects a non-writable named
argument for an `is rw` named param (`g(x => 1)` dies); mutsu currently
binds it silently.
