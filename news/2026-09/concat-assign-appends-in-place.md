# `$s ~= ...` appends in place: an O(n²) accumulation becomes linear

Building a string the way ordinary Raku code builds output — `$s ~= 'x'` in a
loop, which is what template rendering, JSON serialization, report formatting
and HTTP body assembly all reduce to — cost **O(len($s)) per append**, so
producing an n-character string was O(n²). rakudo's is linear, and the gap
grew without bound: 1.8x at 20,000 characters, 11.8x at 80,000, 35.3x at
160,000 and still diverging ([#8695](https://github.com/tokuhirom/mutsu/issues/8695)).

It is linear now, and faster than rakudo across the measured range:

| n | before | after | rakudo |
|---|---|---|---|
| 20,000 | 0.0257s | 0.0066s | 0.0176s |
| 40,000 | 0.0845s | 0.0065s | 0.0191s |
| 80,000 | 0.3122s | 0.0122s | 0.0390s |
| 160,000 | 1.3144s | 0.0579s | 0.0491s |
| 320,000 | — | 0.0512s | 0.0652s |

(Release build, same box, timing the loop only. The empirical exponent was
+1.89 — essentially quadratic; the remaining curve is flat enough that
measurement floor and allocator noise dominate. A figure for a document still
has to come from the bench CI.)

## Why it was quadratic, and why the fix is a fused opcode

`$s ~= 'x'` compiled to four instructions:

```
GetLocalMetaAssign { slot, identity: EmptyStr }   # push the accumulated string
LoadConst("x")
Concat
SetLocal(slot)
```

The read comes *first*, so by the time `Concat` runs the buffer is held twice
— once by the slot, once by the stack — and concatenation can only ever build
a third string. `Interpreter::concat_values` then paid three O(n) passes per
append: `coerce_to_str` materialized the accumulated string, `format!`
allocated and copied both operands into a new buffer, and `is_ascii()` scanned
the *whole* result to decide whether NFC was needed.

The fix is one instruction, `OpCode::ConcatAssignLocal(slot)`, emitted with
the RHS already evaluated. It reads the slot **after** the RHS and *moves* the
value out of it rather than cloning, so the append holds the buffer alone and
can grow it with `String::push_str` — whose geometric growth is what makes the
whole accumulation linear. The fusion is about ownership, not about saving
three dispatches.

## The two things that make it correct

**Uniqueness is checked, not assumed.** `Value::str_appended_unnormalized`
takes its receiver by value and asks `Arc::get_mut`. Any second holder — `my
$b = $a`, a copy pushed into an array, an env mirror of the slot — makes it
copy instead, which is what keeps Raku's value semantics: growing `$a` must
never be visible through `$b`. The opcode also declines the in-place path
outright for a slot that syncs to env, for a container or `Proxy` in the slot,
and for every metadata lane the ordinary scalar store consults (that list now
lives in one predicate, `set_local_scalar_fast_metadata_clear`, which both
paths call, because two copies of it would drift).

**NFC is not append-closed.** Concatenating two separately-normalized strings
is not always normalized: a combining mark at the start of the suffix composes
with the last character of the accumulated string (`"e"` ~ `"\x[301]"` is a
single `é`). Skipping the NFC pass is therefore only sound when the join
cannot compose, so the in-place path requires an **ASCII suffix** — an ASCII
character is a starter and never a combining mark, so it neither composes with
what precedes it nor changes the normalization of anything earlier. A
non-ASCII suffix falls back to the original path unchanged.

## A rakudo-compatibility bug fixed on the way

Reading the accumulator after the RHS is not just convenient, it is what
rakudo does. `$s ~= f()` where `f` assigns to `$s` answers with `f`'s write in
rakudo; mutsu's unfused sequence answered with the value read before the call:

```raku
my $s = 'a';
sub f() { $s = 'ZZZ'; 'b' }
$s ~= f();
say $s;     # rakudo: ZZZb    mutsu before: ab    mutsu now: ZZZb
```

The same divergence still exists for the other compound assignments and for
plain infix operators (`$s = $s ~ f()`, `$i += f()`), which read their left
operand eagerly where rakudo reads it last; only the fused `~=` path is fixed
here.

## What is still quadratic

A **non-ASCII** append (`$s ~= "\c[SNOWMAN]"` in a loop) still takes the old
path, and is still quadratic with a much larger constant — ~6s for 20,000
appends against rakudo's 0.019s. Making that linear needs what the issue
called the subtle half: an incremental "already normalized" state carried on
the string value, plus renormalization of a bounded window around the join
rather than of the whole buffer. That is a change to mutsu's string
representation and is tracked separately.

Pinned by `t/types/string/concat-assign-local-in-place.t` (25 assertions, every one
of them checked against rakudo first): accumulation, aliasing through plain
copies / arrays / hashes / `:=`, composition at the join, the ASCII→non-ASCII
transition, the undefined-LHS identity seed, self-append, the evaluation-order
case above, a readonly target, and a captured accumulator.
