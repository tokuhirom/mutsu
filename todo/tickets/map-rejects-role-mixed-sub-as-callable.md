# `.map()` cannot invoke a role-mixed `Sub` (`&foo but R`)

`dispatch_map_method`'s `is_callable` pre-check
(`src/runtime/methods_dispatch_match2.rs`) matches `func.view()` directly
against `ValueView::Sub(_) | ValueView::Routine { .. } | ...`. A `Sub` mixed
with a role (`&foo but R1`, `&foo.^mixin(R1)`, or a routine composed via a
`trait_mod:<is>` handler's `$r does Role`) is a `ValueView::Mixin` wrapping a
`Sub`, so it fails the check and `.map()` throws `X::Cannot::Map`:

```
$ mutsu -e 'role R1 { method zz(--> True) {} }; sub double($x) { $x * 2 };
            (1,2,3).map(&double but R1)'
Cannot map a Sub to a Seq, it's not callable.
```

Widening `is_callable` to look through a `Mixin` wrapper (matching what
`callframe`/backtrace introspection and `Interpreter::materialize_routine_mixins`
already do — see `news/2026-08/test-assertion-trait-is-not-introspectable.md`)
gets past that check, but the actual per-element invocation inside `.map()`
then fails differently:

```
Callable expected
  in block <unit> at -e line 1
```

So this is two bugs, not one: the `is_callable` classification, and whatever
`.map()` uses internally to invoke each element's callable (which does not
accept a `Mixin`-wrapped `Sub` either). Both need fixing together for
`.map(&foo but R)` to actually work; fixing only the first replaces a clear
`X::Cannot::Map` with a more confusing generic dispatch error, so a genuine
fix must not land the two independently. A repro directly on `raku` confirms
this is expected to work: `raku -e 'role R1 { method zz(--> True) {} }; sub
double($x) { $x * 2 }; say (1,2,3).map(&double but R1)'` prints `(2 4 6)`.
