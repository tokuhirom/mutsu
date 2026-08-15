# `X::Syntax::Malformed.message` drops its leftover class prefix, and a caught `X::Package::Stubbed` no longer re-fires later

Two general bugs found while continuing the `todo/tickets/vendor-real-test-module.md`
campaign (driving rakudo's real `Test.rakumod` against `roast/S32-exceptions/misc.t`).

**`PError::malformed()` stored the full `"X::Type: text"` convention string as
the exception's own `.message` attribute**, instead of just the text portion —
the same double-prefix shape the `X::Anon::Multi`/`InvocantNotAllowed` fixes
elsewhere in this campaign already found and fixed for other classes.
`mutsu -e 'EVAL(q[my $x =])'` reported `.message` as
`"X::Syntax::Malformed: Malformed initializer"`; rakudo's is plain
`"Malformed initializer"`. Fixed by building the `message` attribute from
`format!("Malformed {}", what)` directly, mirroring `PError::raw_with_what`'s
existing `split_typed_message_convention` strip. Pinned by extending
`t/malformed-syntax-classes.t` with a direct `EVAL`+`CATCH` assertion (native
`throws-like` does not check `.message` or its named matchers, so this needed
a real catch, not another `throws-like` line).

**A stub reported by `X::Package::Stubbed` stayed in the interpreter's global
stub registry forever**, since it is only removed when the stubbed
class/package is actually defined. `role Bottle[::T] {...}; class Wine {...};
Bottle[Wine].new` (`Wine` deliberately left a stub) correctly raised
`X::Package::Stubbed` inside an `EVAL`, and a surrounding `try`/`CATCH`
correctly caught it — but the `Wine` entry was still sitting in
`registry().class_stubs` afterward, so the *outer* program's end-of-program
stub check (`check_unresolved_stubs`, run unconditionally in `run.rs`) found
the same stub again and raised the same error a second time, uncaught,
aborting the whole program after the `CATCH` had already handled it. This is
exactly what made `roast/S32-exceptions/misc.t` die mid-file under
`MUTSU_REAL_TEST=1`, well past the assertion that actually exercises the
construct (rakudo raises this once per compilation unit at CHECK time, so a
handled stub error must not resurface later).

Fixed by tracking already-reported names in a **new, separate**
`reported_stub_errors` registry set, consulted by
`check_unresolved_stubs_excluding` alongside its existing `class_stubs`/
`package_stubs` scan. The first attempt removed reported names straight from
`class_stubs`/`package_stubs` instead — that is wrong, caught by
`roast/S12-class/stubs.t` (a genuinely still-stubbed class must keep failing
its *composition* check even after its unresolved-stub *error* has already
been reported once: `class A {...}` reported once, then a later `class B is
A {}` must still see `A` as a stub and raise `X::Inheritance::NotComposed` —
removing `A` from `class_stubs` made that check silently pass instead,
turning a real error into silence). Names are removed from
`reported_stub_errors` wherever a stub is genuinely resolved (defined), so a
name freed up for reuse can report its own fresh error if re-stubbed and
left unresolved again. Pinned by `t/eval-stub-error-not-reraised.t`, green
under `raku` too.

Neither bug was specific to the `Test` module — both are ordinary interpreter
gaps the strict module's stricter assertions exposed, the pattern this
campaign has repeatedly found. `roast/S32-exceptions/misc.t` progresses
further under `MUTSU_REAL_TEST=1` but is not yet fully clean; the remaining
gaps (a handful of individual exception-class/message mismatches) are left
for a future round, per the campaign's own notes in
`todo/tickets/vendor-real-test-module.md`.
