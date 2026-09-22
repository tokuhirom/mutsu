# A non-slurpy `@`-sigil parameter now binds an itemized Range/Seq/List directly

A non-slurpy `@`-sigil parameter (`sub f(@x) {...}`) must bind directly to a
Positional argument, regardless of any `$`-itemization the caller's variable
carries: `my $r = (^5); f($r)` should bind `@x` to the Range's five elements,
exactly as `f((^5))` would. mutsu instead kept the `Scalar` wrapper mutsu
gives a Range/Seq/List assigned into a scalar variable (added so the value
does not auto-flatten in ordinary list context) all the way into `@x`'s own
binding. `@x` itself then still reported the right type, element count and
`.raku` rendering — the wrapper is transparent to those — but a later hyper
op on `@x` (`vm_hyper_ops.rs`'s `exec_hyper_op`) saw the stale itemization
flag and wrapped its whole result in an extra array, doubling the nesting:
`2 <<*<< @x` returned `[(0 2 4 6 8)]` instead of `[0 2 4 6 8]`.

An `Array` argument never showed the bug, because an `Array` is already
non-flattening in list context and so is never `Scalar`-wrapped by
assignment in the first place — only `Range`, `Seq` and `List` values
(which do auto-flatten) get the wrapper. That made the bug invisible to the
common case and easy to miss in the earlier native `Test`/roast-driven
development, since it only shows up when a Range or similar value is
*forwarded* through an intermediate `$`-sigil parameter into a positional
`@`-sigil parameter that later hyper-ops it.

`check_and_coerce_param_type` in `src/runtime/types/binding_signature.rs`
now strips a `Scalar` wrapper from the argument up front for non-slurpy
`@`-sigil parameters, when the wrapped value itself does Positional — before
the existing Seq/LazyList-specific coercions run, so those see the bare
value regardless of how it arrived.

Found via `Math::Polynomial::Chebyshev`'s
`chebyshev-rec(Str:D $t, UInt:D $k, @x, %cheb)` multi candidate, whose
recursive calls forward a Range through a `$method`-adjacent `$x` scalar
parameter before it reaches the `@x` candidate. `Math::Polynomial::Chebyshev`
moved from `red` (0/2 baseline files) to `green` (2/2 files, 32/32
assertions). Regression pinned by
`t/routines/signature/array-param-binds-itemized-range.t`.
