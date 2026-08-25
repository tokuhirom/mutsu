# `Promise.result` on a broken promise mixes in `X::Promise::Broken`

```raku
my $p = Promise.new;
$p.break('oh no');
say $p.result;
CATCH { default { say .^name, ': ', .Str } };
```

- rakudo: `X::AdHoc+{X::Promise::Broken}: oh no`
- mutsu (before): `X::AdHoc: oh no`

## What the reference implementation actually does

Probed against rakudo 2026.06 before implementing, because the ticket's name
("wrapped") suggested the wrong mechanism. `X::Promise::Broken` is a **role**
(`Perl6::Metamodel::ParametricRoleGroupHOW`), and it is composed **at `.result`
time**, not at `.break` time:

- `.cause` hands back the *un*mixed original — `$p.cause.^name` is `X::AdHoc`,
  `$p.cause ~~ X::Promise::Broken` is `False`, and `$p.cause !=== ` the object
  `.result` throws.
- `.result` throws a *new* value with the role composed in, so a user exception
  keeps its own type too: `$p.break(MyEx.new)` makes `.result` throw
  `MyEx+{X::Promise::Broken}`.
- The role overrides **`gist` and only `gist`**. `.Str`, `.message` and
  `.payload` still answer the original cause's text.
- A promise broken with a bare reason has no backtrace of its own
  (`$p.cause.backtrace` is undefined in rakudo); the throw site stamps one on
  the thrown copy.

That last point is what makes the ticket's second repro fall out of the same
fix rather than needing separate machinery: the "Tried to get the result of a
broken Promise" / "Original exception:" text the ticket reports as *missing
entirely* is simply the role's `gist`, which rakudo writes as
`callsame().indent(4)` under a header plus the throw-site backtrace.

## The fix

- `X::Promise::Broken` is registered as a **role** alongside the existing `X::`
  marker roles in `runtime_init.rs` (it was only ever listed in
  `type_constraints.rs`, never registered), so `eval_does_values` can compose it
  and `~~ X::Promise::Broken` answers correctly.
- `dispatch_promise_method`'s `"result"` arm composes the role onto the cause on
  the way out, after stamping the throw-site backtrace when the exception does
  not already carry one (a `die` inside `Promise.start` keeps the backtrace it
  captured on the worker thread — this is a rethrow, not a fresh throw).
- The role's `gist` lives in the new `runtime/promise_broken_gist.rs`. It peels
  *this one role* back out of the value's mixin map — other mixed-in roles are
  kept — and re-dispatches `gist` on the result, which is exactly `callsame()`.
  It is hooked in at the two points every `.gist` funnels through:
  `call_method_with_values` and the VM's `try_native_method` fast path.

## Two general bugs the mixin exposed

Composing a role wraps the instance in a `ValueView::Mixin`, and two places
tested for `ValueView::Instance` directly, so a mixed exception silently lost
its type:

1. `Promise.cause` / `Promise.result` re-wrapped it in a fresh bare `X::AdHoc`,
   which is why the mixin vanished the moment a cause crossed a `.then`
   boundary. Both now go through a shared shape check that looks through mixin
   wrappers.
2. `await`'s `await_died_error` did the same, downgrading an awaited
   `MyEx+{...}` to the generic `X::Await::Died`. It is now mixin-transparent for
   the backtrace append, the message lookup and the pass-through decision —
   which independently fixes `await` on any promise broken with a role-mixed
   exception, not just this one role.

`Interpreter::exception_backtrace_text` also looks through mixin wrappers now,
so a role-mixed exception's backtrace is readable at all its call sites.

## Coverage and known residue

`t/promise-keep-break-semantics.t` pins the type name, the `~~` answers for the
role / base class / `Exception`, the untouched `.Str`/`.message`/`.payload`, the
unmixed `.cause`, the user-exception case, and the three structural properties
of the gist wrapper. It passes verbatim under both `raku` and mutsu.

Two residues are deliberately out of scope and tracked separately:

- The **uncaught** rendering still prints `.message`, not `.gist`, so an
  uncaught `.result` shows `oh no` where rakudo shows the wrapper. This is a
  pre-existing, mutsu-wide divergence — a user `method gist { }` override is
  equally ignored for any uncaught exception — filed as
  `todo/tickets/uncaught-exception-rendering-ignores-gist.md`.
- A cause rethrown from inside a `.then` callback gets an empty backtrace,
  because a worker-thread interpreter carries no source location. The wrapper
  itself renders correctly; only its backtrace lines are absent. Filed as
  `todo/tickets/thread-clone-interpreter-has-no-source-location.md`.
