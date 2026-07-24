# A coercion parameter is only as narrow as the type it accepts

`multi sub f(Str() $s)` used to beat `multi sub f(Blob:D $b)` for a `Blob`
argument, because mutsu ranked a coercion parameter by its *target* type. Raku
ranks it the other way round: a coercion type `T(F)` compiles into **two**
candidates — one that takes `T` directly and one that takes `F` (`Any` when the
source is omitted) and coerces — which roast states outright in
`S06-multi/type-based.t` ("Coercion types introduce two candidates"). Collapsed
into the single candidate mutsu keeps, the rule is: a coercion parameter is as
narrow as its target only when the argument *already is* a target, and is
otherwise as wide open as `F`.

This was not just a wrong answer. `OpenSSL::Digest` declares `md5(Str())`,
`md5(IO::Path:D)` and `md5(Blob:D)`, and its `Str()` candidate delegates to the
`Blob:D` one via `md5 $string.encode`. Picking `Str()` for a `Blob` therefore
looped forever, hanging the module's `03-rsa` and `05-digest` test files.

`candidate_specificity_rank` and `candidate_type_distance` in
`src/runtime/dispatch_candidates.rs` now resolve each parameter's *effective*
constraint against the argument that would bind to it, so ranking has to be
argument-aware (`candidate_specificity_rank_for_args`). Three smaller gaps came
out of the same investigation:

- The Buf/Blob family had no rows in the type-hierarchy distance table, so a
  `Blob:D` candidate scored the "unrelated type" sentinel for a `"x".encode`
  argument and lost the tie-break to whichever candidate was declared first.
- `candidate_dispatch_shape` now shapes a coercion parameter by what it
  accepts, so four all-coercive candidates (`Complex()`, `Num()`, `Rat()`,
  `Int()`) are recognised as tied and reported as `X::Multi::Ambiguous` for a
  `Str` argument, exactly as raku does, instead of silently picking the first.
- An unconstrained positional is an `Any` positional, so `multi f(Any $x)` and
  `multi f($y)` are now an ambiguous pair rather than a silent win for the
  earlier declaration.

Pinned by `t/multi-dispatch-coercion-narrowness.t`;
`t/multi-type-dispatch-regressions.t` had encoded the old guess-the-target
behaviour and was corrected against raku.
