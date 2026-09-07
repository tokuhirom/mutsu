# A raw-invocant write to an immutable value is refused, not dropped

```raku
use v6.e.PREVIEW; use MONKEY-TYPING;
augment class Int { method mut(\S:) { S = 7 } }

my $l = (1, 2);
$l[0].mut;          # raku: dies, "Cannot modify an immutable Int (1)"
                    # mutsu: succeeded, did nothing            (before)
```

`$(1, 2)` (an `ItemList`) behaved the same way, and so did `42.mut` and
`($a + 1).mut` — the ADR-0067 M1/M2 rows, which are the same question asked of
an rvalue. The *observable* half already matched (the caller's data was not
modified), so these were never silent wrong answers, but a program relying on
the refusal ran on.

## Root cause: the marker, not the binder

The ticket expected this to need readonly-ness enforced at the parameter binder.
It did not: mutsu already refuses a raw **parameter** bound to a location-less
value with exactly raku's diagnostic —

```raku
sub g(\S) { S = 7 }
g(5);    # both: X::Assignment::RO: Cannot modify an immutable Int (5)
```

— through the `__mutsu_sigilless_readonly::<name>` marker that
`bind_function_args_values` sets and `OpCode::CheckReadOnly` consults. The raw
**invocant** simply never got that marker: both invocant binders inserted the
value under the parameter's name and stopped there.

So the fix is one marker at each of the two binders —
`call_compiled_method` (which the sigil spellings `$s is raw:` / `$s is rw:`
reach) and `call_compiled_method_fast` (which the sigil-less `\S:` reaches):
set the readonly marker when the invocant arrived with **no location**, clear it
when it did.

"Has a location" is `take_raw_invocant_arrival().is_some() || base.is_container_ref()`.
The second half matters: the `$a.m = v` lvalue path
(`try_raw_invocant_container_lvalue`) hands the caller's container straight in as
`base` without going through the VM's boxing gate, and marking that readonly made
`OpCode::GetLocal` hand back the bare value instead of the container — so
`method mutsuRawInv(\S:) is raw { S }` stopped being an lvalue at all
(`t/raw-invocant-lvalue-container.t`).

## Measured against `raku`, all matching

`$l[0].mut` on a `List` and on an `ItemList`; `42.mut`; `($a + 1).mut`; the
location cases `$v.mut` on a scalar and `@arr[0].mut` on a mutable `Array`,
both of which still write through; and the ordinary store path's own refusals
(`$m[0] = 7`, `@n[0] = 7` through a bound `List`, `(1,2)[0] = 7`), which are
unchanged.

## Testing

New `t/raw-invocant-immutable-write.t` (12 assertions), which passes unchanged
under rakudo. The ADR-0067 pins — `t/raw-invocant-arrives-as-container.t`,
`t/raw-invocant-subscript-receiver.t`, `t/raw-invocant-lvalue-container.t` and
the rest of the `t/raw-invocant*.t` / `t/sigilless*.t` families, 22 files and
293 assertions — are untouched and still pass.
