# Prefix `+`/`-` on a bare type object warns and resumes with the per-type zero

Prefix `+` (numeric coercion) and `-` (negation) applied to a bare type object
now emit the resumable `Use of uninitialized value of type X in numeric context`
warning and resume with the type's own numeric *zero*, matching Rakudo.
Previously mutsu silently resumed a bare `Int 0` with no warning (and `-Any`
wrongly hard-errored).

```
$ raku -e 'my $x = quietly +Num; say $x.raku'      # 0e0
$ raku -e 'my $x = quietly -Num; say $x.raku'      # -0e0
```

The resume value is per type:

| expression | resumes with |
|---|---|
| `+Int` / `+Real` / `+Cool` / `+Any` / `+Str` | `0` (Int) |
| `+Num` | `0e0` |
| `+Rat` | `0.0` |
| `+FatRat` | `FatRat.new(0, 1)` |
| `+Complex` | `0+0i` |

Prefix `-` resumes with the negated zero (`-Num` → `-0e0`).

A class with a user `Numeric` method dispatches even on the bare type object
(`class Foo { method Numeric { 42 } }; +Foo` is `42`, no warning). `Mu` keeps
its hard error (no `prefix:<+>`/`prefix:<->` candidate).

## Implementation

- `Interpreter::type_object_numeric_zero(type_name)` and
  `warn_type_object_numeric_context_resume(type_name, resume)`
  (`src/runtime/io_env.rs`) — the per-type zero and a warning helper that resumes
  with an explicit value (the existing `warn_type_object_numeric_context` now
  delegates with `Int 0`).
- `exec_num_coerce_op` (`src/vm/vm_misc_coerce.rs`) and `exec_negate_op`
  (`src/vm/vm_arith_ops.rs`): a `Package` type object (except `Mu`) dispatches a
  user `Numeric` if present, otherwise warns and resumes with the (negated)
  per-type zero. Defined numeric values skip this entirely, so the hot numeric
  paths are unaffected.

## Known remaining edges (deferred)

- `-Complex` resumes as `0+0i` where rakudo shows `0-0i` (negative-zero on the
  imaginary part — an `arith_negate`/Complex gist display detail, not this path).
- `+Bool` / `-Bool` warn+resume `0` where rakudo throws `X::Multi::NoMatch`
  (`Bool.Numeric` requires `:D`); an extreme edge left as a follow-up.
