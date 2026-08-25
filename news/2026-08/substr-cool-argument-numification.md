# `substr` now numifies its `Cool` offset/length arguments (fixing raku's colon-call gotcha)

The doc-diff harness flagged `Language/syntax.rakudoc:1091`, which illustrates one
of Raku's own traps:

```raku
my $band = 'Foo Fighters';
say $band.substr( 0, 3 ).uc; # OUTPUT: FOO
say $band.substr: 0, 3  .uc; # OUTPUT: Foo
```

A colon call's argument list is a low-precedence listop, so the `.uc` written
after the last argument binds to *that argument* (`3.uc` is the string `"3"`),
not to the call's result. mutsu printed the whole unmodified `Foo Fighters` for
the second line.

## Root cause — not the parser

The obvious suspicion was a colon-call precedence bug, but diffing
`mutsu --dump-ast` against `raku --target=ast` disproved it: both compile the
statement to `substr($band, 0, uc(3))`. mutsu's colon-call listop precedence was
already correct.

The real bug was in `substr` itself. `Interpreter::dispatch_substr`
(`src/runtime/methods_string_substr.rs`) matched the length argument against
`Int` / `Num` / `Rat` / `Sub` (WhateverCode) and fell through to
`_ => total_len` — "no usable length, take the rest of the string" — for
everything else, so a perfectly ordinary `Str` length silently disabled the
length entirely. `$band.substr(0, "3")` therefore returned the whole string. The
offset argument had a sibling defect: its catch-all did a strict
`str.parse::<i64>()` with an `unwrap_or(0)` fallback, so `"3.7"` and `" 4 "`
both silently became offset 0 instead of 3 and 4.

Rakudo's `Cool.substr` candidate coerces both arguments with `.Int`, so a numeric
string is parsed (truncating toward zero), a `Bool` becomes 0/1, and a
non-numeric string raises `X::Str::Numeric`.

## Fix

A shared `substr_cool_to_i64` helper numifies a `Cool` offset/length the way
rakudo does: strings go through `parse_raku_str_to_numeric` (so `"3.7"` is 3 and
`" 4 "` is 4), other `Cool` values through the shared `coerce_to_numeric`, and a
string that is not a valid number raises `X::Str::Numeric` with raku's own
message shape instead of being silently treated as 0 or as "take the rest".
`Str` / `Bool` / enum arguments now use it in both the offset resolver and the
length arm; `Whatever` and `WhateverCode` keep their existing meanings.

With that, `$band.substr: 0, 3 .uc` reproduces raku's gotcha exactly (`Foo`), and
so do `$band.substr(0, '3')`, `$band.substr('3.7', 2)` and
`$band.substr(0, True)`.

## Tests

`t/colon-call-argument-parsing.t` pins the trailing-`.method` binding for the
colon-call form, the parenthesized counterpart (where the trailing method binds
to the whole call), a single-argument variant, and the `Cool` numification cases
directly. It passes verbatim under both `raku` and `mutsu`.
