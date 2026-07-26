# A successful `try` should leave `Any` in `$!`, not `Nil` — blocked on strict-mode undeclared variables

Split out of `todo/tickets/bang-var-timing-in-try-catch.md` on 2026-07-26. The
other half of that ticket (the CATCH block seeing the in-flight exception under
`$!`) is fixed — see `news/2026-07/error-var-try-catch-timing.md`. This half was
implemented, found to regress a whitelisted roast file, and reverted.

## Repro

```raku
try { 1 };
say $!.^name;
# raku:  Any        mutsu: Nil
```

`Nil` is the *initial* `$!` of a scope, and what a handled CATCH restores; `Any`
is specifically what a `try` that completed without an error stores. It matters
after an earlier failure too — `try { die }; try { 1 }` must clear `$!` to `Any`,
and mutsu leaves it at `Nil`.

The one-line change is in `src/vm/vm_try_catch_ops.rs` (the `Ok(())` arm of
`exec_try_catch_op_inner`, marked with a `TODO` pointing at this file): store
`Value::package(Symbol::intern("Any"))` instead of `Value::NIL`.

## Why it is blocked

The two values behave differently when a method is called on them: `Nil.foo`
returns `Nil` (in raku too), while `Any.foo` dies with "No such method". So the
change turns every "`$!` was never set, and something called a method on it"
into a die — which is correct, and is exactly what raku does, but it un-masks
places where **mutsu failed to raise an error in the first place**.

`roast/S32-exceptions/misc2.t` (whitelisted) is one:

```raku
try EVAL('$i-just-made-this-up = "yup"');
is +($!.suggestions), 0, "no suggestions for a strange variable";
```

raku's EVAL fails with `X::Undeclared` ("Variable '$i-just-made-this-up' is not
declared"), so `$!` holds a real exception there. **mutsu has no strict-mode
undeclared-variable error at all** — `mutsu -e '$x = 5; say $x'` prints `5`, and
reading an undeclared variable yields `Nil` — so the EVAL succeeds, `$!` stays
unset, and the file only passed because `Nil.suggestions` happened to return
`Nil` (`+Nil` == 0). With `Any` it dies mid-file and 14 subtests never run.

So this is really two findings:

1. **mutsu does not implement the strict-mode undeclared-variable error.** That
   is the blocking prerequisite, and it is a deep change — mutsu's loose
   variable model is long-standing, the compiler would have to distinguish a
   genuinely undeclared lexical from `our`/dynamic/magic/`CALLER::`-inherited
   names, and any code (including `t/`) that relies on the looseness breaks.
2. Once that lands, the `Any` change is a one-liner plus these pins.

## Pins to add when it lands

`t/error-var-try-catch-timing.t` has a NOTE where the two assertions were
removed; re-add them:

```raku
sub ok-try() { try { 1 }; $!.^name }
is ok-try(), 'Any', 'a try that completes without an error leaves Any';

sub ok-after-failing() { try { die "boom" }; try { 1 }; $!.^name }
is ok-after-failing(), 'Any', 'a successful try clears an earlier exception to Any';
```

`t/exception-methods.t`'s last block also carries a NOTE: its two assertions
(`$!.message` / `$!.line` return Nil when there was no error) only hold while
`$!` is `Nil`; under raku those calls die, so that block becomes
`is $!.^name, 'Any'` + `dies-ok { $!.message }`.
