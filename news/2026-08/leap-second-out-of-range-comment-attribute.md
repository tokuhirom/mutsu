# `X::OutOfRange` for an invalid leap second now carries `.comment`

`t/out-of-range-scalar-index.t` was the last open item in the
"compile errors that name no exception class" batch
(`todo/tickets/compile-errors-that-name-no-exception-class.md`, now closed —
the other eight closed earlier via later exception-class work). It reached the
right exception class (`X::OutOfRange`) already, but under the real vendored
`Test` module (`MUTSU_REAL_TEST=1`) one assertion still failed:

```raku
throws-like 'DateTime.new(year => 2012, month => 5, day => 22, hour => 18, minute => 3, second => 60)',
    X::OutOfRange, 'leap second', comment => /'leap second'/;
```

`DateTime.new(..., second => 60)` on a day that is not a UTC leap-second
insertion day raises raku's `X::OutOfRange` with `.comment` set to
`"a leap second can occur only at 23:59"`, alongside `.what`/`.got`/`.range`.
mutsu's `validate_datetime` (`src/builtins/methods_0arg/temporal.rs`) raised
this specific rejection as a bare string-message error with none of those
structured attributes at all (the other month/day/hour/minute/second-range
rejections in the same function correctly have no `.comment` — this one
specifically does, in raku).

## Fix

Added `make_leap_second_out_of_range_error`, a dedicated constructor (via
`RuntimeError::typed("X::OutOfRange", attrs)`) that sets
`what => "Second"`, `got => <the rejected second value>`,
`range => "0..^60"`, and `comment => "a leap second can occur only at
23:59"`, matching raku's exact values — verified directly against `raku` for
this construction, not just the pinned test's `.comment.match(/leap second/)`
check.

`t/out-of-range-scalar-index.t` now passes fully under `MUTSU_REAL_TEST=1`.
