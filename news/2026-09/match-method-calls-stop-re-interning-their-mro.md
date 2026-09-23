# A method call on a `Match` no longer rebuilds the `Match` MRO

Issue #8888 is about what a method call on a *builtin* receiver costs before
the method itself runs. Its first slice (#8957) added the `(kind, method)`
table for plain arrays, hashes and strings. This slice measured the receivers
that table does not cover, and found the most expensive one.

## Where method calls actually are

The obvious next target was `bench-json-fast`, which the previous slice
reported as "not moved". `MUTSU_VM_STATS=1` shows why: the whole benchmark
makes **2,575** method calls against 198,488 `NqpOp`s. JSON::Fast is written
in `nqp::` ops, so widening the method table cannot move it, and this slice
does not try. Across `benchmarks/`, the files that do make method calls are
`bench-class`, `bench-ctor` (user classes, #8880's side) and
`bench-regex-capture`, which calls `.from`/`.to`/`.Str`/`.chars` on `Match`
objects.

## The cost

`$/.from + $0.Str.chars` in a 100,000-iteration loop was **4,887M
instructions**, about 48,900 per iteration. A fifth of it was one function:
`Registry::class_mro_readonly` answered the MRO of `Match` (and of `Capture`,
the `IO::Spec` family, `CompUnit`, ...) from a hardcoded `&'static [&str]`
table, and turned it into symbols with `Symbol::intern` plus a fresh `Arc` on
**every** call. A `Match` method call asks for that chain several times: the
fast accessor probe, `has_user_method`, `grammar_has_user_method`,
`is_native_method`. The catalog-driven branch next to it had been given a
once-per-process cache in #7766; this branch had not.

## The fix

- `interned_builtin_mro` memoizes each table row's interned chain per thread,
  keyed on the static slice's address and length. Each `match` arm is a
  promoted constant, so a key is shared only by rows the compiler merged,
  which are identical chains.
- `has_user_method`, `grammar_has_user_method` and
  `resolve_user_method_or_accessor` get `_sym` variants. The compiled dispatch
  entries already hold the method name as a `Symbol`, but these gates took a
  `&str` and interned it again, about twelve interns per iteration of the
  loop above. The `&str` forms delegate to the new variants. Two
  `native_lever_a_user_override` calls on the `CallMethodMut` path switch to
  the existing `_sym` form for the same reason.
- The type-object guards at the top of `try_compiled_method{,_mut}_or_interpret`
  decode the receiver's `Package` symbol once instead of once per guard.
  Measured on its own this is noise (-0.15% on `bench-ctor`), because the
  `"new"` string compare already short-circuited most of them. It is kept
  because it is simpler, not because it is faster.

## Numbers

Callgrind, `--profile profiling`, same box, before and after:

| | before | after | |
| --- | ---: | ---: | ---: |
| `$/.from + $0.Str.chars` x 100k | 4,887,154,385 | 3,827,273,250 | **-21.7%** |
| `bench-regex-capture.raku` | 1,367,228,873 | 1,311,129,841 | **-4.1%** |
| `@a.elems` x 100k | 832,724,996 | 805,340,351 | -3.3% |
| `$o.m()` x 100k (user class) | 1,664,413,768 | 1,663,631,126 | -0.05% |
| `bench-ctor.raku` | 1,560,965,965 | 1,556,653,523 | -0.3% |
| `bench-class.raku` | 1,096,822,447 | 1,092,973,753 | -0.4% |

A `Match` method call is still about 38,000 instructions per loop iteration,
spread over the same layers #8888 describes. `try_hash_storage_delegate_mut`,
`try_env_pure_scalar_native_dispatch` and `cstruct_class_name` each show up as
a few percent of that loop for a receiver none of them can claim. They are
the next things to look at.

`t/oo/class/builtin-mro-table-cache.t` pins the MRO answers the memo now serves,
including interleaved lookups of different table rows.
