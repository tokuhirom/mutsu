# `==` falls back to `.Numeric`, so two DateTimes naming one instant compare equal

`roast/S32-temporal/DateTime.t` regressed under `MUTSU_REAL_TEST=1` on the
subtest `can parse leap second in non-UTC timezones`, whose twenty assertions
are all of the form

```raku
cmp-ok utc.in-timezone($_), '==', utc, "parsed correct date for .in-timezone($_)";
```

## It was surfaced by a fix, not caused by one

The file was clean at the start of the session and appeared in the closing
sweep, so it looked like a regression from that day's work — and in a sense it
was, but the underlying defect was older. PR #7096 made the **routine** form of
a numeric comparison (`&infix:<==>($a, $b)`, the only form the vendored
`Test.rakumod`'s `cmp-ok` ever uses) share one implementation with the operator
form. Before that, the routine form went through a separate static fold that
happened to numify two `DateTime`s and answer `True`. The operator form
`$a == $b` had been answering `False` all along; unifying them simply made the
wrong answer visible to `cmp-ok`.

So the fix belongs in `==` itself:

```raku
my $utc = DateTime.new('2016-12-31T23:59:60Z');
say $utc.in-timezone(7200) == $utc;   # was False, rakudo says True
```

## The rule mutsu was missing

`DateTime` does **not** do `Real` or `Numeric` — in rakudo either
(`$dt ~~ Real` and `$dt ~~ Numeric` are both `False` there too). What makes the
comparison work is rakudo's last-resort candidate,
`multi infix:<==>(Any \a, Any \b) { a.Numeric == b.Numeric }`.

mutsu's Instance→numeric bridge (`coerce_infix_operand_numeric`) only numifies
an object that does `Real`/`Numeric` **or** has a *user-written* `Numeric`
method. `has_user_method` cannot see a native one, so `DateTime` fell straight
through to structural equality — and two `DateTime`s naming the same instant in
different timezones differ structurally, hence `False`.

`num_eq_values` now applies the `.Numeric` fallback itself when the bridge has
left both operands as objects, using the result only if **both** numify to
something that is no longer an object. An object with no `.Numeric` therefore
still reaches the existing structural path unchanged.

## Scoped to `==`/`!=` on purpose

The first attempt widened the shared numeric bridge instead, which is the
obvious place. It was measured and reverted: the bridge also serves `-`, `<=>`,
`cmp` and the arithmetic operators, and numifying a `DateTime` there destroys
their type-specific behaviour — `DateTime - DateTime` stopped being a
`Duration` and `DateTime <=> DateTime` stopped ordering, breaking three further
assertions in the same roast file under *both* providers. rakudo has real
candidates for those; only the equality family reaches the generic `Any, Any`
one. The pin asserts the `-` and `<=>` behaviour precisely so a future widening
cannot quietly repeat that.

Pin: `t/numeric-equality-falls-back-to-the-numeric-method.t` (10 assertions,
green under real `raku` as well as mutsu). A divergence the pin documents but
cannot assert — an object with **no** `.Numeric` makes rakudo die where mutsu
answers `False` — is filed as
`todo/tickets/numeric-op-on-an-object-without-numeric-answers-instead-of-dying.md`.
