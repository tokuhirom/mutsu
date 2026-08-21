# `is Hash` subclass instances have essentially no Array-subclass-style method delegation

Found while fixing
`todo/tickets/assign-pos-direct-call-not-mutating-array-subclass-instance.md`
(a direct `.ASSIGN-POS(...)` call not mutating an `is Array` subclass
instance). That ticket speculated the `is Hash` twin (`.ASSIGN-KEY`) has "the
identical gap" from "the same synthetic-binding mechanism" and should be
fixed in the same PR. Investigation showed this is false: `is Array`
subclasses have a whole delegation subsystem for Instance receivers
(`src/vm/vm_call_method_mut_ops.rs`, the ~250-line `CallMethodMut`
Array-subclass delegation block starting ~line 2263, keyed off the
`__mutsu_array_storage` attribute and `Self::is_positional_base` /
`Self::positional_base_storage` in `src/runtime/accessors_state.rs`) — and
`is Hash` subclasses have **no equivalent at all**. There is no
`__mutsu_hash_storage`-style attribute, no `is_associative_base` helper, and
no Hash-subclass block in `vm_call_method_mut_ops.rs` or the non-mut
`CallMethod` path (`vm_call_method_ops.rs`) — `grep -rn
"__mutsu_hash_storage\|is_associative_base"` across `src/` returns nothing.

## What actually happens today

`class Bar is Hash {}` behaves inconsistently depending on how the instance
is constructed and accessed:

```raku
class Bar is Hash {}

my $h1 = Bar.new;              # no named args
say $h1.WHAT.gist;             # raku: (Bar)   mutsu: (Hash)  <- wrong class!
say $h1.^name;                 # raku: Bar     mutsu: Hash    <- wrong class!
$h1{'a'} = 1;                  # "works" only because $h1 is secretly a plain
say $h1<a>;                    #  Hash value, not a Bar Instance at all, so
                                #  every Hash method just dispatches natively.

my $h2 = Bar.new(a => 1, b => 2);  # named args -> a REAL Bar Instance
say $h2.WHAT.gist;             # (Bar)  -- correct this time
say $h2.^name;                 # Bar    -- correct this time
say $h2<a>;                    # raku: 1   mutsu: (Any)        <- subscript READ broken
$h2.AT-KEY('a');                # raku: 1   mutsu: "No such method 'AT-KEY' for
                                #            invocant of type 'Bar'"
$h2.ASSIGN-KEY('a', 99);        # raku: sets it   mutsu: "No such method
                                #            'ASSIGN-KEY' for invocant of type 'Bar'"
```

So there are really two separate, pre-existing bugs bundled together here:

1. **`Bar.new` (zero-arg / positional-only constructor path) does not produce
   a `Bar` Instance at all** — it produces a plain `Hash` type-object/value
   whose `.^name` reports the *parent* class `Hash`, not `Bar`. This is why
   `t/mut-method-receiver-writeback-coherence.t`'s existing `Bag2 is Hash`
   subtests ("hash-backed instance element assign is coherent") pass despite
   the gap below: `Bag2.new` with no named args silently degrades to a bare
   `Hash`, so subscript assignment/read just hits the ordinary native Hash
   path, never touching Instance/attribute machinery at all. This may itself
   be a constructor-dispatch bug worth its own investigation (does
   `is Array` have the same `.new`-with-no-args degradation? A quick check
   suggests not — `Foo.new` for `class Foo is Array {}` does report `.^name`
   as `Foo` — so this looks Hash-specific, possibly in how the default
   `Hash`/`Map` `.new` multi candidate resolves/blesses when composed into a
   subclass with no attributes of its own).
2. **When a `Bar` Instance genuinely is constructed** (e.g. via
   `Bar.new(a => 1, b => 2)`, which routes through a different `.new`
   candidate that actually blesses an Instance), there is no method
   delegation for ANY Hash-ish method — not just the mutating ones
   (`ASSIGN-KEY`, `BIND-KEY`, `DELETE-KEY`), but read-only ones too
   (`AT-KEY`) and even subscript syntax (`$h2<a>` returns `(Any)` instead of
   erroring or working). This needs a whole new delegation subsystem parallel
   to the Array one: an attribute name to hold the backing `Hash`/`Map` value
   (analogous to `__mutsu_array_storage`), a `Self::is_associative_base`
   helper (analogous to `Self::is_positional_base`), a
   `CallMethodMut`/`CallMethod` delegation block keyed off it, AND the
   subscript-read/subscript-write index-op paths
   (`vm_var_index_ops.rs`/`vm_var_assign_index_named.rs`/
   `vm_var_assign_element.rs`) taught to recognize an `is Hash` Instance
   receiver the way they (presumably) already recognize an `is Array` one.

## Why this is `todo/deep/`, not a small ticket

This is not "add one match arm" — it is standing up an entire parallel
subsystem (attribute storage convention + MRO-base helper + a ~250-line
delegation block + subscript-op integration) for Hash the way it already
exists for Array, PLUS a separate, independent constructor-dispatch bug
(finding 1 above) that needs its own root-cause investigation before the
delegation block would even be reachable for the common `Bar.new()` case.
Doing this properly is comparable in size to the original Array-subclass
delegation work, not a copy-paste of the two-line ASSIGN-POS fix that landed
for the Array case.

## Repro

Both snippets above reproduce against the current `main` (verified with a
built `target/debug/mutsu`, compared against real `raku`). No existing `t/`
regression pins the `Bar.new(a => 1, b => 2)` (real-Instance) case — only the
`Bar.new` (degrades-to-plain-Hash) case is covered by
`t/mut-method-receiver-writeback-coherence.t`, and that test does NOT
exercise the gap because it never triggers the Instance path.

## Suggested next steps

1. Root-cause why `Bar.new` (no named args) for `class Bar is Hash {}`
   produces a bare `Hash` value instead of a `Bar` Instance, while
   `Bar.new(a => 1, ...)` (with named args) does produce a proper `Bar`
   Instance. Compare against the `is Array` `.new` dispatch, which appears
   not to have this split.
2. Once `Bar.new` reliably produces a `Bar` Instance, design and implement the
   Hash-subclass-Instance delegation subsystem: a `__mutsu_hash_storage`
   (name TBD) attribute convention, `Self::is_associative_base`, a
   `CallMethodMut`/`CallMethod` delegation block mirroring the Array one, and
   subscript-read/write integration.
3. Add `t/hash-subclass-*.t` regression coverage mirroring
   `t/array-subclass-*.t` once implemented.
