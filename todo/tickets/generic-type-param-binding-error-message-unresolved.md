# A generic `::T` type parameter is not resolved when reporting later type-check failures

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Parameter.rakudoc:306`).

## Repro

```raku
sub c(::T $x, T $y, $z) { my T $zz = $z };
c(4, 5, 6);          # OK

try c(4, 5, "six");
given $! { .message.say };

try c("four", 5, "six");
given $! { .message.say };
```

- raku:
  ```
  Type check failed in assignment to $zz; expected Int but got Str ("six")
  Type check failed in binding to parameter '$y'; expected Str but got Int (5)
  ```
- mutsu (`target/debug/mutsu`):
  ```
  Type check failed in assignment to $zz; expected T but got Str ("six")
  Calling c(Str, Int, Str) will never work with declared signature (::T $x, T $y, $z)
    X::TypeCheck::Binding::Parameter: Type check failed for y: expected Str, got Int
  ```

## Analysis

Two distinct symptoms, both stemming from the same root cause — mutsu never resolves the
generic type-capture `::T` (bound from the first call argument's actual type) when later
checking `T`-typed declarations against it:

1. First `try`: `c(4, 5, "six")` binds `::T` to `Int` (from `$x = 4`). The `my T $zz = $z`
   assignment then fails its type check (`$z = "six"` is a `Str`), and the error message
   should report the *resolved* type (`expected Int`), but mutsu prints the literal type-capture
   name `T` instead — `T` was never substituted with the value bound at the `::T $x` parameter.
2. Second `try`: `c("four", 5, "six")` binds `::T` to `Str` (from `$x = "four"`), so `$y`
   (typed `T`, i.e. now `Str`) should fail binding against the `Int` argument `5` with a plain
   single-parameter binding error. Instead mutsu produces a completely different error shape —
   a multi-line "Calling c(...) will never work with declared signature ..." message plus a
   different exception class (`X::TypeCheck::Binding::Parameter` with a differently-worded
   message) — suggesting the failed `T` comparison falls through to a generic
   "no signature matches" style diagnostic (as if this were multi-dispatch resolution) instead
   of the ordinary single-sub parameter-binding type-check path.

## Affected files (starting point)

- Wherever `::T`-style generic type captures are resolved and threaded through parameter/variable
  type checks (look for `type_captures` handling — also referenced in the `Type/Parameter.rakudoc`
  reflection code at `src/value/signature.rs`).
- The binding-failure error-reporting path for a single (non-multi) sub call, to see why a `T`-typed
  parameter mismatch takes a different code path than an ordinary type mismatch.

## Suggested next step

Compare `--dump-ast`/trace for a working `::T`-generic assignment type check against this failing
one to find where the resolved concrete type gets dropped, and where the second `try`'s error
path diverges into the multi-dispatch-shaped diagnostic.
