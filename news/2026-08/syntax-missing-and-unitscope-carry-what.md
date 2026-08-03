# `X::Syntax::Missing` and `X::UnitScope::*` carry their `.what`

Naming the class is only half of a typed exception. `throws-like 'repeat { 1 }',
X::Syntax::Missing, what => '"while" or "until"'` reads the *attribute*, and the
`"X::Type: text"` parse-error message convention
(`news/2026-08/parse-error-keeps-its-exception-class.md`) preserves only the
class. So both of these matched the class and then died on
`No such method 'what'`, aborting the rest of the file — the same shape the
earlier typed-attribute pass fixed for five other classes
(`news/2026-08/typed-exceptions-carry-their-attributes.md`).

Three changes, in increasing order of generality:

**`X::Syntax::Missing` derives `what` from its own message.** rakudo spells that
message as literally `Missing {what}`, so the attribute is a strip of the
prefix rather than a second copy — the message and the attribute cannot
disagree, and a future raise site gets `.what` for free. Done once in
`RuntimeError::exception_value_with_backtrace`, the bridge that turns the
message convention into an exception object. The `repeat` site's wording moved
to rakudo's (`Missing "while" or "until"` rather than `"while" or "until"
required after repeat`), which is what makes the derivation yield the right
value there; `Missing block` already had the shape.

**`X::UnitScope::TooLate` / `X::UnitScope::Invalid` carry it explicitly.** Their
messages do not spell `what` in a derivable place, so the four raise sites pass
it through a new `PError::raw_with_what`. It stays a *soft* error — these sites
are best-error candidates the statement dispatcher may still back out of, so
making them fatal would change parsing.

**A soft parse diagnosis now forwards its structured exception.** `parse_program`
only copied `PError::exception` into the `RuntimeError` on the *fatal* branch;
the soft branch rebuilt from the message alone and dropped it. That is why the
attribute above did not arrive until this was fixed too.

Two whitelisted roast files now run to completion under `MUTSU_REAL_TEST=1`
instead of aborting mid-plan: `roast/S04-statements/repeat.t` (21 tests) and
`roast/S06-other/main-semicolon.t` (10 tests).

Pin: `t/syntax-missing-and-unitscope-carry-what.t`. It reads `.what` off the
caught exception rather than going through `throws-like`'s named matchers —
mutsu's own native `throws-like` does not check those, so a matcher-based pin
would have passed without the fix. Without it the file stops at its second
assertion; all 9 pass under `raku`.
