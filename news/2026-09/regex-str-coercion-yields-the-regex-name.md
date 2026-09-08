# Str-coercing a named regex yields its name, not the empty string

`~&foo` for a `regex`/`token`/`rule` routine now produces `"foo"`, matching
rakudo. It used to produce the empty string — for *every* named regex — which
made any two of them compare equal as strings and none of them equal to
anything else.

## What was wrong

`regex_str_coercion` implements rakudo's rule that a `Regex` in string context
warns ("Regex object coerced to string (please use .gist or .raku to do that)")
rather than handing back its source text. mutsu resumed that warning with the
empty string unconditionally. rakudo resumes it with the regex's `.name`:

| | raku | mutsu (before) |
| --- | --- | --- |
| `~&foo` (`my regex foo { a }`) | `foo` | `` |
| `~&tok` (`my token tok { b }`) | `tok` | `` |
| `~&rul` (`my rule rul { c }`) | `rul` | `` |
| `~rx/x/` | `` | `` |
| `~rx:i/x/` | `` | `` |

Only the *anonymous* shapes are empty, and for the ordinary reason that a
nameless `Code`'s `.name` is empty. `&foo.Str` already answered `"foo"`, so the
two coercions disagreed with each other as well as with rakudo.

## How it surfaced

`roast/S02-magicals/sub.t`'s "`&?ROUTINE` is correct inside a regex/token/rule"
compares the routine captured inside the regex against the regex itself with
`is`. The vendored upstream `Test`'s `is` compares with `$got eq $expected`,
which Str-coerces both sides — so all three subtests were comparing `""` with
`""`... and then failing anyway, because the *other* side of the comparison
(`&?ROUTINE` inside a regex, which mutsu still hands back as a `Sub` rather
than a `Regex`) stringified through the routine path and gave `"foo"`. The
diagnostic printed two identical-looking strings.

The empty string was never a safe answer: it makes every named regex compare
`eq` to every other one.

## Fix

`regex_str_coercion` resumes with the routine's name for the
`Routine { is_regex: true, .. }` shape and keeps the empty string for the two
anonymous ones (`ValueView::Regex` and `ValueView::RegexWithAdverbs`). The
warning itself is unchanged, as is `.gist`/`.raku`, and the reason the
coercion refuses to return the *source* — a smartmatch of a `Regex` against a
`Regex` stringifies the LHS to give the RHS a subject, and `"/a/"` really does
contain an `a`.

Pinned by `t/regex-str-coercion-name.t` (10 tests, every expectation measured
against raku first).

## Still divergent, not fixed here

- `&?ROUTINE` inside a regex code block returns a value whose `.WHAT` is `Sub`
  where rakudo's is `Regex`. It stringifies and compares correctly now, so the
  roast test passes, but the type is still wrong.
- `G.^lookup('TOP')` on a grammar token returns something that stringifies as
  `Regex()` rather than `TOP`.
- `rx/x/.name` is `Nil` where rakudo's is `""`.
