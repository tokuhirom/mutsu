# Red t/41-fallback: lazy `Seq.grep`, scoped `my $*OUT` in methods, `:@x` unpack re-reads a Seq

Three independent gaps kept Red's `t/41-fallback` red:

- `Seq.grep` on a materialized Seq ran the matcher eagerly. It now defers like the Range and Array receivers, so a `return self.Seq.grep: &f` from a callee runs `&f` under the consumer's dynamic scope.
- A `my $*OUT` declared inside a method leaked to the caller under its `$*OUT` key (the `*OUT` spelling was restored, `print` reads the other one). The method-frame merge now treats `$*x` as frame-local when `*x` is a compiled local.
- A named `@` target in a destructuring signature (`for @tests -> (:@expected!)`) lowered to an explicit `.list`, which consumes a `Seq`; unpacking the same hash twice died with `X::Seq::Consumed`. It now uses the re-reading contextualizer form.

Red 0.2.7 serial release run: 79/97 files rc=0 (78 before).
