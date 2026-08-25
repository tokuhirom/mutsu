# Native builtin methods reject an extra named argument instead of swallowing it via the implicit `*%_`

Found while establishing colon-call / colonpair precedence against raku for
`t/colon-call-argument-parsing.t`.

Every Raku *method* has an implicit `*%_` slurpy named parameter, so passing a
named argument that the method does not declare is silently accepted. mutsu
honours this for user-defined methods but not for native builtin methods: the
extra named argument makes the arity-based native dispatch miss entirely, and the
call fails with a `No such method` error (complete with a misleading
"Did you mean ...?" suggestion).

## Repro

```raku
say 4.log(:base(2));   # raku: 1.3862943611198906   mutsu: No such method 'log' for invocant of type 'Int' / Did you mean 'log2'?
say "abc".uc(:foo);    # raku: ABC                  mutsu: No such method 'uc' for invocant of type 'Str' / Did you mean 'fc'?

class C { method m() { 42 } }
say C.new.m(:foo);     # raku: 42   mutsu: 42   -- user-defined methods are already correct
```

The adverbial spelling reaches the same path, so `say 4.log :base(2)` and
`say 4.log:base(2)` (an extended method name in raku) diverge too.

Note that *subs* correctly reject an unexpected named argument in both
implementations (`sub s() { 42 }; s(:foo)` dies in raku and in mutsu) — only
methods carry the implicit `*%_`, so the fix must not be applied to sub dispatch.

## Root cause (starting point)

`call_method_with_values()` picks a native method by *arity*
(`native_method_0arg` / `native_method_1arg` / ...), counting the named argument
as a positional-slot occupant. A named argument that no native method declares
should be dropped from the arity count before the native lookup (and, for the
handful of native methods that do accept named adverbs — `split`'s
`:skip-empty`, `substr-eq`'s `:i`/`:m`, `.comb`'s adverbs — left in place), then
either ignored or exposed as `%_`.

## Affected files

- `src/runtime/methods.rs` (`call_method_with_values`)
- `src/builtins/methods_0arg/`, `src/builtins/methods_narg.rs` (arity tables)
- `src/runtime/did_you_mean.rs` (the misleading suggestion is downstream of the
  same miss)

## Why it is not trivial

The named-argument set that each native method genuinely consumes is implicit in
the per-method Rust code rather than declared anywhere, so "drop unknown nameds
before arity dispatch" needs a way to distinguish a consumed adverb from an
ignorable one without regressing the methods that do read adverbs.
