# A reduce metaop over an empty list returns `Nil` instead of an `X::NoZeroArgMeaning` `Failure` for operators that have no identity

Found while verifying `news/2026-08/reduce-x-xx-left-associative.md` (the `[x]`/`[xx]`
associativity fix). It is unrelated to associativity — it reproduces on `main` before that
fix — so it was not folded into that PR.

## Repro

```raku
say ([x] ()).raku;
```

- `raku`: `Failure.new(exception => X::NoZeroArgMeaning.new(name => "infix:<x>"), ...)`
- `mutsu`: `Nil`

Operators that *do* have a well-defined identity element already agree, which is the useful
contrast:

```raku
say ([~] ()).raku;   # both: ""
say ([+] ()).raku;   # both: 0
```

## What Raku specifies

Reducing an empty list yields the operator's identity element. An operator with no meaningful
identity (`infix:<x>` among them — there is no value `v` such that `v x n` is a no-op for all
`n`) instead returns a `Failure` wrapping `X::NoZeroArgMeaning`, so the divergence only shows
up once the empty-list case is actually reached. mutsu returns a bare `Nil` for the whole
no-identity class rather than the typed `Failure`.

Because it is a `Failure` and not a thrown exception, the divergence is silent until the value
is used: `[x] ()` sinks harmlessly in mutsu where raku would explode on the unhandled `Failure`.

## Scope to establish before implementing

The fix is not just `infix:<x>`. Enumerate which core infixes raku gives a zero-arg meaning to
and which it does not (`raku -e 'say ([OP] ()).raku'` across the infix table), and drive mutsu's
reduce from that same classification rather than special-casing `x`. Check the triangle form
(`[\x] ()`) and the `&infix:<...>` spelling too. `X::NoZeroArgMeaning` also needs to exist as a
real exception type in mutsu's taxonomy with the `name` attribute raku populates.

## Affected files (starting point)

- `src/runtime/builtins_reduce.rs` — the reduce fold and its empty-input arm.
- `src/vm/vm_misc_ops.rs` — the VM-side reduce path with its parallel classification table.
