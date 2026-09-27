# `.name` and `.package` are no longer answered by every object

A generic fallback answered `.name` and `.package` for any receiver (#9776).
`3.name` was `Nil`, `"x".name` was `"x"`, `Any.name` was `"Any"`, and
`$undefined.package` was `Nil`. So code that meant `.^name` kept running with
a wrong value instead of dying.

These methods are declared on `Code`. Only `Array`/`Hash` also answer `.name`,
from their container descriptor. Everything else now raises
`X::Method::NotFound` as rakudo does: an `Int`, a `Str`, a `List`, a `Pair`, a
`Bool`, and any type object, which is named in the message (`for invocant of
type 'Int'`).

Four cases keep an answer:

- Routines keep their name and package.
- An anonymous regex's name is `""`.
- `Nil` still swallows the call.
- A `Code` type object (`Sub.name`) reports that it has no attribute to read, as
  it does in rakudo.

One local test had pinned the old behaviour (`Plain.name` living); it now
expects `X::Method::NotFound`.

`.^can('name')` now agrees with the call and is empty for every receiver except
`Code`, where it used to list the ClassHOW method. Removing the fallback exposed
an older bug that the `Nil` answer had been hiding: a `Match` smartmatched
against a `Str` (`$/[0] ~~ 'distro'`, and so `given $/[0] { when 'distro' { ... } }`)
was always `False`. It now compares the matched text, which zef's
`SystemQuery` relies on.
