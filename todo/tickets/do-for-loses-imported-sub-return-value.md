# `do for` loses the return value of an imported module sub

Found 2026-07-25 while pinning the sigilless-parameter/native-type-name fix
(`news/2026-07/sigilless-param-shadows-native-type-name.md`). Independent of that
fix — it reproduces with an ordinary sigiled signature.

## Repro

`tmp/mlib/MyPlain.rakumod`:

```raku
unit module MyPlain;
sub plainsub (Str $s) is export { "P:$s" }
```

```raku
use MyPlain;
say do for ^2 { plainsub('x') };        # raku: (P:x P:x)   mutsu: ((Any) (Any))
say do for ^2 -> $i { plainsub('x') };  # raku: (P:x P:x)   mutsu: ((Any) (Any))
say do given 1 { plainsub('x') };       # both: (P:x)
say plainsub('x');                      # both: P:x
```

Only `do for` is affected. `do given` collects correctly, a plain `for` loop with
an explicit `.push` collects correctly, `.map` collects correctly, and a
**locally declared** sub of the same shape collects correctly under `do for`:

```raku
sub localsub($s) { "L:$s" }
say do for ^2 { localsub('x') };        # both: (L:x L:x)
```

So the failing combination is precisely *`do for`* + *a call to an imported
routine* as the loop body's last statement. The collected value is `Any`, i.e.
the body's value is read from somewhere that the imported-sub call path does not
write.

## Where to look

`do for` collects each iteration's value; a local-sub call and a method call both
land it where the collector reads, while the imported-sub call does not. The
likely seam is the loop-body value capture (`vm_for_loop_*`, the `do`-prefix
value collection) versus the return path used for a module/dynamic sub — which
is the interpreter `call_function_def` route rather than a compiled call, and so
publishes its result differently.

## Impact

Silent wrong data, not an error — a `do for` over imported routines yields a list
of `Any`. Any script that builds a list this way from a module's exported subs is
affected. Found while writing `t/sigilless-param-named-like-native-type.t`, whose
loop assertion had to be rewritten as an explicit `for` + `.push` to avoid it.
