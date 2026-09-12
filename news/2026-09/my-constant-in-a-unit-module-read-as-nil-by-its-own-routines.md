# A `my constant` in a `unit module` read as Nil from that module's own routines

`Business::CreditCard` classified every card number as `NotACreditCard`. The
distribution's suite ran to completion — 39 of 87 assertions passed, so nothing
crashed — but `cardtype` never recognised anything. Both of the two bugs behind
that turned out to be general interpreter gaps with nothing to do with credit
cards, and the distribution is green on both counts now.

## A `constant` lost depending on how its scope was spelled

`Business::CreditCard` builds four lookup tables at compile time:

```raku
my constant @lookup     = do { ... };
my constant @renamed    = NotACreditCard, AmericanExpress, ...;
my constant %CUPcountry = <US MX AI ...>.map(* => Discover);
my constant %JCBcountry = <US PR VI ...>.map(* => Discover);
```

Read from `cardtype`, every one of them was `Nil` — so `@lookup[$card.chars]`
was falsy and the `elsif`/`else` chain fell through to `NotACreditCard`.
Reduced, the gap is not about `do` blocks, `.Map`, or binding into array
elements, all of which worked:

```raku
# lib/M.rakumod
unit module M;
my constant @my-k   = 10, 20, 30;
constant    @bare-k = 40, 50, 60;
our sub probe() {
    say @my-k.raku;    # raku: (10, 20, 30)   mutsu: Nil
    say @bare-k.raku;  # raku: (40, 50, 60)   mutsu: (40, 50, 60)  -- fine
}
```

The two spellings behaved differently, for every sigil. A `unit` compunit's
file-scope `constant`s are package symbols of that compunit rather than names
the importer sees bare (#7787), so `import_module` strips their plain `env`
binding and leaves the value in `module_scope_names` — which is exactly where
the module's own routines read them from. Separately, the compunit's `my`
lexicals are moved out of `env` into `unit_lexicals`, and are *removed* from
`module_scope_names` so there is a single authoritative store for them.

A `my constant` matched both collectors, and the `my`-lexical one ran second.
The value was taken out of the store its own routines read and filed as a
compunit lexical instead, so every read answered `Nil`.

The two collectors disagreed because they identified a `constant` differently.
`collect_unit_package_scope_names` discriminates on the `__constant` trait, and
documents why: a `constant` parses as `VarDecl { is_our: true }`, which `is_our`
alone cannot tell from an ordinary `our $x`. `collect_unit_lexical_names`
skipped constants only incidentally — via that same `is_our: true` — and `my
constant` parses with `is_our: false`, so it slipped straight through.
`collect_unit_lexical_names` now tests the `__constant` trait too, which is the
fact it meant to test all along rather than a scope keyword that happened to
line up.

## An enum value as an array subscript

With the tables populated, `t/01-original-tests.rakutest` went from 22 failures
to 1 and `t/02-cpan-tests.rakutest` still failed 28 of 30. The remaining
failures all reached the last line of `cardtype`:

```raku
$obsolete ?? $found !! @renamed[$found];
```

`@renamed` had its 36 elements and `$found.Int` was a valid index, but
`@renamed[$found]` was `Any` — and `@renamed[$found.Int]` was correct. An enum
value is a `Cool`, so as a positional subscript it numifies to its value;
mutsu numified neither side of the subscript protocol:

```raku
enum Color <Red Green Blue>;
my @a = 'zero', 'one', 'two';
say @a[Green];      # raku: one     mutsu: Nil
my @b = 'a','b','c';
@b[Green] = 'X';    # raku: ok      mutsu: Index out of bounds
```

The read path left the enum alone. The write path went through
`index_to_usize`, whose last resort is to parse the value's string form — which
for an enum is its *key* (`"Green"`), so the parse failed and the write reported
"Index out of bounds". Both sides numify now, gated on the subscript being
positional: an associative subscript keeps the enum value itself as the key
(`%h{Green}` is keyed by the enum, not by its ordinal), and numifying there
would have broken the matching write.

Two neighbours of that rule came along with it, because they are the same rule:

* `Bool` is `enum Bool <False True>`, so `@a[True]` is `@a[1]`. It reaches the
  subscript as a plain `ValueView::Bool` rather than a `ValueView::Enum`, so it
  had the identical gap on both paths and needed its own arm on each.
* A *string*-valued enum (`enum E (S => 'x')`) must NOT numify. `EnumValue`'s
  existing `as_i64` answers `0` for one, because its callers want a total
  function — folding `@a[S]` to element 0 would be a silently wrong read where
  raku dies with `X::Str::Numeric`. Both subscript sites use a new
  `EnumValue::as_index_i64`, which is `Option`-returning precisely so the
  non-numeric case stays on the pre-existing path instead of inventing an
  answer. Negative-valued enum members fall through the same way, and are
  rejected as out of range exactly as a literal `@a[-1]` is.

The 01 suite mostly survived this second bug because it passes `:obsolete`,
which takes the ternary's other branch and never indexes `@renamed`.

## Result

`Business::CreditCard` 0.2 goes `red` -> `green`: 2 of 2 baseline files, 87 of
87 assertions, matching rakudo exactly. Pinned by
`t/modules/import-export/unit-my-constant-visible-to-own-routines.t` (both
spellings, every sigil, plus the #7787 non-leak assertions that must keep
holding) and `t/types/enum-subset/enum-value-as-positional-index.t` (read,
write, slice, autovivification, explicitly-valued enums, `Bool`, the
string-valued enum that must *not* fold to 0, and the associative subscript that
must *not* numify).

One finding from the same investigation is filed rather than fixed: a `-->`
return type naming an enum value (`multi sub cardtype($, *% --> NotACreditCard)
{ }`) returns `Nil` instead of that value, while `--> 42` and `--> "lit"` are
correct (#8022). The distribution's own suite does not exercise that candidate,
and the discriminator involved is shared by a compiler and a runtime copy that
have to agree, so it is a slice of its own.
