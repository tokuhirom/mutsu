# `Str.match`'s `:x` adverb validates its value's type

`"foobar".match("o", :x<hello>)` silently ignored the adverb and returned `｢o｣`
where Rakudo reports `X::Str::Match::x: in Str.match, got invalid value of type
Str for :x, must be Int or Range`. Found by the doc-diff harness on
`Type/X/Str/Match/x.rakudoc:15` — the exception type that page documents was
never produced.

## Root cause

The validation *existed*. `is_valid_match_x_arg` and `str_match_x_error` were
already in `src/runtime/seq_helpers/regex_captures.rs`, and both `.match` and
`.subst` already called them. The accept-list was simply too wide: it included
`ValueView::Str(_)`, so `:x<hello>` passed validation, and
`parse_match_repeat_bounds` then returned `None` for it — leaving the match to
run unbounded, i.e. exactly as if `:x` had not been passed.

Probing `raku` for the real rule (rather than trusting the doc's "Int or Range"
prose) showed the boundary is `Numeric`, not `Int`:

| `:x` value | raku |
|---|---|
| `2`, `1..2`, `*` | accepted |
| `True` | accepted, counts as 1 (`Bool` is an `Int`) |
| `<2>` (an `IntStr` allomorph) | accepted — it *is* `Numeric` |
| `2.0`, `1.5` | accepted; a fractional bound truncates toward zero |
| `"2"`, `"hello"` | **rejected** — a plain `Str` is not `Numeric`, even when it spells a number |
| `(1, 2)`, a `Hash` | rejected |

So the fix is not "reject anything non-Int"; it is to drop `Str` from the list
and add the numeric flavours that were missing (`BigInt`, `Bool`, `Rat`,
`BigRat`, `FatRat`). A numeric allomorph is a `ValueView::Mixin` in mutsu, so
`is_valid_match_x_arg` and `parse_non_negative_int` now unwrap that first —
without it, `:x(<2>)` would have been rejected as an `IntStr`, trading one wrong
answer for another.

Two smaller faithfulness fixes came out of the same probe:

- **`.match` returns the `Failure`; `.subst` throws.** `my $r = "ab".match("a",
  :x<z>)` gives a `Failure` in Rakudo (`$r.^name` is `Failure`) and only blows up
  when the result is used, while `.subst` throws eagerly. mutsu threw from both.
  `dispatch_match_method` now returns `str_match_x_failure(...)`.
- **The message always names `Str.match`.** Rakudo's `X::Str::Match::x` says
  `in Str.match, …` even when the adverb arrived through `.subst`; mutsu
  substituted the calling routine's name and said `in Str.subst, …`. The
  `routine` parameter is gone.

`parse_non_negative_int` also learned `Bool` and the rational views, so
`:x(True)` selects 1 match and `:x(1.5)` truncates to 1 — matching Rakudo's `.Int`
coercion of the adverb rather than falling through to "no bound at all".

Pinned by `t/str-coercion-and-dispatch.t`.
