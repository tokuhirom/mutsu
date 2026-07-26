# A CATCH block no longer sees the in-flight exception under `$!`

The first half of a divergence recorded on 2026-07-25 while fixing the
caller-`$!`-clobbering bug (`news/2026-07/caller-error-var-survives-a-call.md`)
is fixed.

## What was wrong

```raku
try {
    die "x";
    CATCH { default { say $!.defined ?? $!.message !! "Nil" } }
}
# raku:  Nil        mutsu (before): x
```

In Raku the CATCH block has its own `$!`, which starts out `Nil` — inside the
handler the exception is the *topic* (`$_`), and the enclosing scope's `$!` has
not been written yet; it is written when the `try` completes. mutsu wrote `$!`
before running the handler, so the handler saw it. The same held for a CATCH in
a bare block, and it did not depend on the scope already holding an earlier
exception.

## The fix

In `src/vm/vm_try_catch_ops.rs`, the CATCH dispatch now sets `$!` to `Nil`
before running the handler (the topic still gets the exception) and publishes
the exception into the enclosing `$!` only *after* the handler finishes without
matching. Everything else is preserved: a matching `when`/`default` — or a
`.resume` — restores the pre-`try` `$!`, and an unmatched explicit CATCH still
rethrows.

`src/vm/vm_misc_scope.rs`, listed as a suspect in the original note, turned out
to be the LEAVE/POST phaser `$!` plumbing rather than a CATCH twin — bare-block
CATCH goes through the same dispatch, so both forms were fixed in one place.

Two local tests encoded the old, raku-incompatible behaviour and were corrected
to read the topic instead: `t/untyped-error-adhoc.t` (`$ex = $!` inside CATCH)
and `t/vm-basic.t` (`is $!, "boom"` inside CATCH). Both assert the same thing
about the exception; they just read it from where raku puts it.

Pin: `t/error-var-try-catch-timing.t` (13 assertions, each verified against raku
first): the initial and fresh-routine `$!`, an untrapped `die`, a handled CATCH
in both `try` and bare-block form, `$!` and the topic inside a CATCH block, an
inner `try` publishing to the enclosing body, and an unmatched CATCH rethrowing.

## The other half is split out, and it found a second gap

The same note also reported that a *successful* `try` leaves `Nil` where raku
leaves the `Any` type object. That change was implemented and then reverted: it
regresses the whitelisted `roast/S32-exceptions/misc2.t`, because `Nil.foo`
returns `Nil` while `Any.foo` dies — so it un-masks a place where mutsu fails to
raise an error at all. **mutsu has no strict-mode undeclared-variable error**
(`mutsu -e '$x = 5; say $x'` prints `5`), so that file's
`try EVAL('$i-just-made-this-up = "yup"')` succeeds, leaves `$!` unset, and only
passed because `+Nil` is `0`. Both findings are recorded in
`todo/tickets/successful-try-leaves-any-not-nil.md`, with the one-line change
and the pins to re-add once the strict-mode prerequisite exists.
