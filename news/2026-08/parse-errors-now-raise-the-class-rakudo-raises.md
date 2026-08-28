# Parse errors raise the class rakudo raises, and `throws-like`'s parse-error leniency is retired

Nine compile-time diagnoses collapsed into a generic `X::Syntax::Confused` (or,
worse, an `X::AdHoc`). They passed CI only because mutsu's **native**
`throws-like` deliberately broadened its type check: it accepted any error whose
message contained `"Confused"` / `"parse error"` whenever the expected class
started with `X::Syntax`, plus a similar `X::Comp` / `X::Comp::Group` widening.
The real `Test.rakumod` (`MUTSU_REAL_TEST=1`, `todo/deep/vendor-real-test-module.md`)
compares `$_ ~~ $expected` and reported every one of them as a failure.

All nine now raise the class `raku` raises, each re-derived against `raku`:

| snippet (inside `EVAL`) | before | after (= raku) |
| --- | --- | --- |
| `@arr [0]` | `X::Syntax::Confused` | `X::Syntax::Missing` |
| `42.:all` | `X::Syntax::Confused` | `X::Syntax::Number::IllegalDecimal` |
| `say 42.:all` | `X::Syntax::Confused` | `X::Syntax::Number::IllegalDecimal` |
| `"${$scalar}"` | `X::AdHoc` | `X::Obsolete` |
| `"@{$array}"` | `X::AdHoc` | `X::Obsolete` |
| `rt54804( 1, , 3, )` | `X::Syntax::Confused` | `X::Syntax::InfixInTermPosition` |
| `{my $foo; $^foo;}(1)` | `X::AdHoc` | `X::Redeclaration` |
| `{*.{}}()` | `X::Syntax::Confused` | `X::Syntax::Malformed` |
| `'RT' ~~ m\c[SNOWMAN].\c[COMET]` | `X::Syntax::Confused` | `X::Comp::Group` |

## What each one actually was

**A `[...]` in infix position is the reduce metaoperator.** Its content therefore
has to name an infix, and rakudo says so — `Missing infix inside []`, with
`what => 'infix inside []'`. mutsu read `[0]` as a user-defined infix named `0`,
failed to find a right operand, and reported that instead. The user-infix branch
of `parse_list_infix_loop_impl` now emits rakudo's diagnosis when the bracketed
name is not an in-scope `infix:<…>`.

**`<digit>.` always sorrows, but only sometimes groups.** rakudo raises
"Decimal point must be followed by digit" as a *sorrow*, then retries the
leftover `.` as a postfix; when that retry succeeds the sorrow is its only
complaint and is thrown alone, and when the retry panics too both are bundled
into an `X::Comp::Group`. mutsu had only the group. Now `42. i`, `42. foo` (a
method name after whitespace) and `42.:all` (the `.:name` reified-operator
postfix, which really is valid syntax — `$x.:all` parses fine in raku) raise the
lone `X::Syntax::Number::IllegalDecimal`, while `42.`, `42.,`, `42.:`, `42.:1`
keep the group. That also fixed `minimal-whitespace.t` #9, which expects
`X::Comp` and was getting an `X::Comp::Group` — not a subclass of it in rakudo.

**The Perl 5 dereference blocks were diagnosed as a string.** `${$x}` / `@{$x}`
outside a string already raised a real `X::Obsolete`; inside `qq` interpolation
the parser pushed `die "X::Obsolete: …"` — an ordinary string — so `$!` saw an
`X::AdHoc` and only the message sniffing kept the class alive. The interpolation
path now embeds the real exception instance, and both spellings carry rakudo's
`.old` / `.replacement` naming the construct as written
(`${$scalar}` / `$($scalar) for hard ref or $::($scalar) for symbolic ref`).

**A comma cannot begin a term.** `primary()` already seeded
`X::Syntax::InfixInTermPosition` for a leading `=>`; `,` is exactly the same
story, so an empty list slot (`(1, , 3)`, `f(1, , 3)`, `my @a = 1, , 2`) now gets
it too. Like the `=>` seed it stays SOFT and scores zero consumed input, so it
only surfaces once every alternative has failed.

**One placeholder branch returned a string where its siblings returned
instances.** `check_placeholder_conflicts` builds real `X::Placeholder::NonPlaceholder`
and `X::Undeclared` instances for two of its three outcomes; the
`my $foo` + `$^foo` case returned `"X::Redeclaration: …"` as text. It now builds
the instance, with `.symbol` / `.what` / `.postfix` matching rakudo.

**`{*.{}}()` needed two fixes.** A `*`-curry wrapped in braces is already a
closure, and rakudo rejects it (`Malformed double closure; WhateverCode is
already a closure without curlies…`) — mutsu accepted it and silently produced a
block returning a `WhateverCode`, so `<foo bar>.map({ * + 1 })` gave two
`WhateverCode`s instead of an error. The check fires exactly where rakudo's does:
a bare `{ }` term (or bare block statement) whose SOLE statement curries. `{ * }`
(the proto stub), `{ *, 1 }` (a bare `*` does not curry), `{ * + 1; 2 }` (two
statements), `-> $x { * + 1 }`, `sub { * + 1 }` and `if 1 { * + 1 }` all stay
legal, as verified against raku. Separately, `.{}` and `.[]` — the *dotted* zen
slice — did not parse at all (`%h.{}` was "Confused" even though `%h{}` worked),
so the block body collapsed before the diagnosis could fire. Both now parse, and
`*.{}` / `*[]` curry into a `WhateverCode` that returns the container.

**An unterminated regex with a non-ASCII delimiter had nowhere to go.**
`delim_commits_to_regex` committed only for `/ { [ ( <`, on the reasoning that
`m-bar` is an ordinary identifier. A non-ASCII *symbol* has no such ambiguity —
`m☃…` cannot be an identifier and cannot be `m` followed by an operator — so it
commits too, and the existing "Regex not terminated" `X::Comp::Group` fires
instead of the parse backtracking out to "Bogus postfix: ☃".

## The leniency is gone, and the real bug was reading the wrong object

With the classes right, the widening in
`src/runtime/test_functions/throws_like.rs` was retested. Deleting the
message-substring branches outright broke **20 whitelisted files**, all on
`right exception type (X::Comp)` / `(X::Syntax::Confused)` — because those errors
carry no structured exception in `RuntimeError::exception`, so `ex_class` was
`None` and the class/MRO/role branches were skipped entirely.

`throws-like` was already computing the right object elsewhere: its *named
matchers* answer off `err.exception_value()`, which derives a real instance from
the `"X::Type: text"` convention or the parse code — which is why `$!.^name`
reported `X::Syntax::Confused` all along while the type check saw nothing. The
type check simply read a different field than the matchers did.

Pointing both at `exception_value()` makes the widenings dead, and they are
deleted: the `X::Syntax::Confused` branch, the `starts_with("X::Syntax")` branch,
the `X::Comp` / `X::Comp::Group` branch, the
`expected == "X::Comp::Group" && class_does_role(cls, "X::Comp")` broadening, and
the `expected == "X::AdHoc"` "matches any ad-hoc error" catch-all. A full sweep
of all **1436 whitelisted roast files is green** with all five removed, so the
native provider's type check now asks the same question the real module does,
against the same object.

## Verification

`t/parse-error-exception-classes.t` pins all nine classes plus the zen slice and
the sorrow/group split; it passes under real `raku` unchanged as well as under
mutsu. `make test` is green (3520 files, 35081 tests). The seven affected roast
files pass under **both** the native provider and `MUTSU_REAL_TEST=1`
(`S02-types/whatever.t` keeps only its two `# TODO` failures), and the full
1436-file whitelisted roast sweep is green on a release build. The roast-side
regression count under the real `Test` module drops from 55 to 46.
