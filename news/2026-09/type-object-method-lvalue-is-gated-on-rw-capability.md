# A type-object method lvalue is gated on rw-capability, not silently swallowed

`$Class.m($arg) = $v` where `m` is not rw-capable must die in raku. mutsu
reported success and dropped the write, after re-calling `m` with the assigned
value — or, for a sigilless parameter, with the *invocant*:

```
$ mutsu -e 'class N { method m($x) { say "called with ", $x; $x } }; my $a = 42; N.m($a) = 5; say $a'
called with 5          # the ASSIGNED value bound into the method's first parameter
42                     # exit 0: the assignment silently did nothing

$ mutsu -e 'class N { method m(\x) { say "called with ", x; x } }; my $a = 42; N.m($a) = 5; say $a'
called with (N)        # a sigilless param re-read by source name and got the INVOCANT
42
```

The **instance** twin was already correct — `N.new.m($a) = 5` raises
`X::Assignment::RO: method 'm' is not rw` — because the instance paths hit that
refusal before the legacy chain.

## Why the type object fell through

`assign_method_lvalue_with_values` tries the ADR-0059 lvalue return first, then
falls back to mutsu's legacy `$obj.name($value)` setter convention. That
fallback is guarded by `setter_convention_would_preempt_lvalue_return`, which
deliberately answers `false` for a non-instance target: for a type object the
lvalue return has *already* run, at the top of the function, and declined — so
the doc comment reasoned that "the remaining legacy chain is all that is left to
try and must not be blocked".

That reasoning holds for the shapes the legacy chain genuinely serves. It also
let a plainly non-rw-capable method be re-called as a setter, whose `Ok(result)`
then reported the assignment as done.

## The oracle the instance path already had

The fix is not to block the chain — blocking alone would turn a silent no-op
into a different silent no-op — but to give the type object the same *positive*
path the instance has. `try_rw_method_container_lvalue`, which already owns the
type-object invocant, now handles the non-rw branch too: it calls the method
**once, with its real arguments**, and assigns through the result. A container
comes back and the write lands; a plain value comes back and that is raku's
`Cannot modify an immutable Int (42)`.

The gate is `Interpreter::method_is_rw_capable` — the same declaration oracle
(`is rw` / `is raw` / an explicit `return-rw`) ADR-0067 slice 2 added for the
instance path — so the blast radius is exactly "a method the class declares".
Everything the class does not declare answers `Ok(None)` and the legacy chain is
untouched: a builtin, a name resolved elsewhere, the `AT-KEY`/`AT-POS` element
accessors, an argument-less `Class.m() = $v` (the legacy chain needs a non-empty
argument list, so there is nothing to preempt), and the attribute-accessor shape
`method x() { $!x }`, which names its location rather than computing one.

That answers the ticket's open question — what the type-object legacy chain is
still *for*. It is not for declared methods; those now go through the oracle,
and the chain keeps only the cases where mutsu has no declaration to consult.

## Scope

Both spellings now die where raku dies, and the sigilless one is byte-identical
(`Cannot modify an immutable Int (42)`). For the `$`-parameter spelling raku
reports `Cannot assign to a readonly variable or a value` where mutsu reports
the immutable-value wording; rakudo picks its message from *which* readonly
thing it found, and mutsu's instance path has its own wording too
(`X::Assignment::RO: method 'm' is not rw`). Aligning the whole family's message
text is a separate job from making the assignment fail.

`t/method-rw-capability-oracle.t` grew from 22 to 26 assertions: the row that
recorded the weaker fact (the caller variable is untouched) and pointed here is
now a real `dies-ok` refusal, joined by the `$`-parameter twin and by a row
asserting the method sees its real argument exactly once.
