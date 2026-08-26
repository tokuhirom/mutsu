# `Date`/`DateTime` field range checks throw `X::Temporal::OutOfRange`

Found by the doc-diff harness batch-4 re-run (`Type/DateTime.rakudoc:137`).

## What was wrong

mutsu already registered `X::Temporal::OutOfRange` as a proper subclass of
`X::OutOfRange` that `does X::Temporal` — introspectable, but never actually
thrown. Every range-check builder in `builtins/methods_0arg/temporal.rs`
constructed the generic parent instead, so a
`CATCH { when X::Temporal::OutOfRange { ... } }` written against the documented
type caught nothing:

```raku
say DateTime.new("2012-02-29T12:34:56Z").clone(year => 2015);
CATCH { default { put .^name, ': ', .Str } };
# raku:  X::Temporal::OutOfRange: Day out of range. Is: 29, should be in 1..28
# mutsu: X::OutOfRange: ...
```

## The fix — and where the family actually ends

The ticket named the `day`/`second` builders. Checking each field against
`raku` first showed the boundary is not where it looked: rakudo raises
`X::Temporal::OutOfRange` for the *date-field* checks and leaves the rest as
the plain `X::OutOfRange`.

| rejected field | rakudo |
|---|---|
| `Month`, `Day` (`Date` and `DateTime` alike) | `X::Temporal::OutOfRange` |
| `Hour`, `Minute` | `X::Temporal::OutOfRange` |
| `Second`, including the leap-second rejection | `X::OutOfRange` |
| `minutes of timezone` | `X::OutOfRange` |

So `Second` and the timezone check — which come out of rakudo's signature and
`Instant` checks rather than its date-field validator — were deliberately left
alone. Blanket-converting the file, which is what the ticket's "switch them to
`X::Temporal::OutOfRange`" suggested, would have made two of the six wrong in
the other direction.

`make_out_of_range_error_int` split into a temporal and a plain flavour over a
shared body, with the boundary documented at the builder. The messages are
unchanged (both classes render the same text), so only the catchable type
moves.
