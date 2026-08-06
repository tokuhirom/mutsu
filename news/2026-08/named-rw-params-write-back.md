# Named `is rw` parameters write back to the caller's variable

```raku
sub g(:$x! is rw) { $x = 7 }
my $v = 1;
g(x => $v);
say $v;   # raku: 7 — mutsu printed 1
```

The named-parameter arm of `bind_function_args_values` had no
`is rw`/`is raw` handling at all: it never registered a writeback and never
aliased, so the body's write stayed in the callee env. Found while landing
shared-cell rw binding for positionals
(`news/2026-08/rw-params-bind-shared-cells.md`); fixed by extending the same
mechanism to the named arm. A named `is rw`/`is raw` scalar param now binds
the caller's shared `ContainerRef` cell (source resolved from the
`key=source` arg-sources encoding or a VarRef on the Pair's value), registers
the `rw_bindings` entry for the call-site slot resync, and — matching raku —
a non-writable named argument for an `is rw` param dies with
`X::Parameter::RW` at bind time (`is raw` still accepts literals read-only).

Residuals (kept in scope notes, not blocking): raku's compile-time
"Cannot use 'is rw' on optional parameter" check (mutsu accepts `:$x is rw`
without `!`), and hash/array *element* named sources (`g(x => %h<a>)`
writes back in raku; mutsu's named sources only carry variable names).

Pinned by `t/named-rw-param.t` (7 cases, verified against raku).
