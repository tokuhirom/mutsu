# `Pointer[T].raku` uses a bare type name and named-arg form instead of raku's fully-qualified positional form

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Language/nativetypes.rakudoc:172`).

## Repro

```raku
use NativeCall;
sub malloc( int32 $size --> Pointer[void] ) is native { * };
my Pointer[void] $for-malloc = malloc( 32 );
say $for-malloc.raku;
```

- `raku`: `NativeCall::Types::Pointer[NativeCall::Types::void].new(297902560)` (the
  trailing number is the pointer address, inherently non-reproducible/allocator-
  dependent — not itself a bug to match).
- `mutsu` (`target/debug/mutsu`): `NativeCall::Types::Pointer[void].new(address =>
  128219605755824)`.

Two deterministic (non-address) format differences, independent of the actual pointer
value:
1. The type parameter renders as the bare `void` instead of the fully-qualified
   `NativeCall::Types::void`.
2. The constructor call renders as a named argument (`.new(address => N)`) instead of
   raku's positional form (`.new(N)`).

## Affected files (starting point)

- Wherever `Pointer[T]`'s `.raku` method is implemented (grep for `Pointer` in
  `src/runtime/` NativeCall support) — needs to (a) fully-qualify the type-parameter
  name the same way the outer `Pointer` type itself already is, and (b) emit the
  constructor call positionally rather than as a named `address` pair.
