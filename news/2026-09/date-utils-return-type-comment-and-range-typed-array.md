# Date::Utils: two general fixes for a `#`-comment in a return type and a Range assigned to a subset-typed array

Working the `Date::Utils` ecosystem distribution (locked on
[#7884](https://github.com/tokuhirom/mutsu/issues/7884)) turned up two
general interpreter bugs, both fixed and pinned under `t/`.

## A `#`-comment after `-->` leaked into the parsed return type

`Date::Utils`'s signatures write their return type with an inline
explanatory comment:

```raku
sub day-index-in-week(
    DoW $dow, # range 1..7
    DoW :$cal-first-dow = 7, # Sunday
    :$debug,
    --> UInt # range 0..6
) is export {
    my DoW @list = days-of-week $cal-first-dow;
    for @list.kv -> $i, $v {
        return $i if $v == $dow
    }
}
```

`parser::parse_return_type_annotation` scanned the raw bytes between `-->`
and the signature's closing punctuation without skipping `#` comments, so
the parsed return-type spec came out as the string `"UInt # range 0..6"`
instead of `"UInt"`. That malformed spec then failed
`is_definite_return_spec`'s known-type-name check and was misclassified as
a definite-return-*value* spec (the same family as `--> Nil` or `--> 42`),
so the `return $i` in the body raised a spurious compile error:

```
Runtime error: No return arguments allowed when return value UInt # range 0..6 is already specified in the signature
```

Fixed by excluding comment spans from the annotation text as
`parse_return_type_annotation` scans it, mirroring how `ws()` already skips
comments everywhere else in the parser. Pinned in
`t/routines/signature/signature-return-type-comment.t`.

Grepping the ecosystem ledger for the same error message surfaced three more
distributions blocked by exactly this shape — `DateTime::US`,
`Holidays::Miscellaneous`, and `Holidays::US::Federal` — all of which moved
from `blocked_load` to `partial` on re-measure without any distribution-side
change.

## A finite `Range` assigned to a subset-typed array checked a sentinel, not the real elements

Separately, `Date::Utils` declares:

```raku
subset DoW of Int is export where { 0 < $_ < 8 }
...
my DoW @dow = 1..7;
```

`exec_type_check_op_inner`'s handling of a `Range` assigned to a `@`-sigilled
typed array took a shortcut: instead of checking the Range's actual
elements, it spot-checked the element constraint against a sentinel value
(`0` for an integer Range). A subset whose `where` clause excludes exactly
that sentinel — as `0 < $_ < 8` excludes `0` — rejected the *whole*
assignment even though every element of `1..7` legitimately satisfies `DoW`:

```
Type check failed in assignment to @dow; expected DoW but got Range
```

Fixed by reifying a finite `Range`/`GenericRange` into a real array up
front, alongside the array's existing `HyperSeq`/`RaceSeq`/`Slip`
reification, so the normal per-element check applies. Pinned in
`t/types/enum-subset/typed-array-range-init-subset.t`.

## Result

`Date::Utils` 0.7.0: `blocked_load` -> **`green`**, 7/7 baseline files,
90/90 assertions pass.

## Filed separately

Investigating the neighbouring `"No return arguments allowed"` records also
found a **different**, unrelated bug: a `subset` declared with a
**lowercase** name (e.g. `Net::BGP::IP`'s `subset ipv6_int of UInt where *
< 2¹²⁸;`) used as a return type is still misclassified, because sub
declarations are hoisted and validate their return-type spec before an
earlier-in-source `subset` statement has actually executed and registered
the type. Filed as
[#8657](https://github.com/tokuhirom/mutsu/issues/8657) (`todo:deep`) rather
than fixed here — it needs a decision about sub/subset registration
ordering, not a local classifier tweak.
