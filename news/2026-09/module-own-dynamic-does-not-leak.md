# A module's own file-scope `my $*x` no longer leaks into the importer

A module body executes against the *caller's* `env` for historical reasons.
Nothing cleaned up a dynamic variable the module declared for **itself** at
file scope, so it stuck around forever:

```
$ mutsu -I lib -e 'EVAL q[use Own]; say ($*MODULE_PRIVATE // "(undeclared)")'
from-module          # real Raku: (undeclared)
```

Two existing mechanisms almost covered this, but neither actually did:

- `collect_unit_lexical_names`, which moves a `unit` compunit's own
  file-scope `my` out of `env` and into `unit_lexicals`, explicitly
  *excludes* dynamics — a `$*x` is dynamically scoped by definition, not a
  compunit lexical.
- The plain-env restore that undoes the module's writes after it runs
  explicitly *skips* dynamic keys (added for
  [#8229](https://github.com/tokuhirom/mutsu/issues/8229), so that a
  module's write to a dynamic the *caller* already owns — `$*PACKAGE_LOADED++`
  reporting a load-time fact — survives the load).

Between the two, a dynamic the module declares for **itself** fell through
both nets and was simply left behind.

The fix collects the module's own top-level `my $*x`/`@*x`/`%*x`
declarations and, after the module body runs, restores or removes exactly
those keys like any other plain name, while every dynamic the module only
*wrote to* (never declared) keeps the #8229 behavior. The actual `env` key
for a scalar dynamic keeps its `$` sigil at file scope (`$*x`), while the
declaration's own AST name is already sigil-less — matching goes through a
small sigil-stripped comparison rather than an exact key match. This applies
whether or not the module has a `unit` declarator, since a bare-file module
runs its whole body against the caller's `env` the same way.

See [#8241](https://github.com/tokuhirom/mutsu/issues/8241) and the
regression test `t/modules/module-own-dynamic-does-not-leak.t`.
