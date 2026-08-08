# A private method call that fails to bind is reported as "No such private method"

```raku
class C {
    method !p(Int $n) { $n * 2 }
    method go() { self!p("not-an-int") }
}
C.new.go;
```

```
raku:   Type check failed in binding to parameter '$n'; expected Int but got Str ("not-an-int")
mutsu:  No such private method 'p' for invocant of type 'C'
```

Same for arity:

```raku
method !p(Int $n) { 1 }
self!p(1, 2)
```

```
raku:   Too many positionals passed; expected 2 arguments but got 3
mutsu:  No such private method 'p' for invocant of type 'C'
```

The **public** forms of both are already right — `self.q("s")` reports
`X::TypeCheck::Binding::Parameter` and `self.q(1, 2)` reports "Too many
positionals passed". Only the `!`-private path substitutes a not-found error.

## Mechanism

`resolve_private_method_any_owner` / `resolve_private_method_with_owner`
(`runtime/resolution_private_method.rs`) pick a candidate by *matching the
arguments against each overload's signature*. When nothing matches they return
`None`, and the caller
(`runtime/methods_instance_ops.rs`, the `make_method_not_found_error(pm_name,
…, true)` at the end of the private-dispatch arm) cannot tell "the class has no
such private method" from "the one candidate exists but the arguments do not
bind".

A fix has to keep the real distinction: with several candidates, a genuine
no-match is `X::Multi::NoMatch`; with exactly one candidate, the binding error
from attempting it is what raku reports. The public dispatch path already makes
that distinction, so the shape to copy is there.

## Why it matters

It is actively misleading, not just cosmetic. Cro's `CookieJar.add-from-response`
calls `self!get-cookie-lifetime($_, $state)` in a loop; a *separate* bug left
`$_` holding a `Bool` instead of a `Cro::HTTP::Cookie`, and the resulting
signature mismatch was reported as `No such private method
'get-cookie-lifetime' for invocant of type 'Cro::HTTP::Client::CookieJar'`.
That sent the investigation into the method-registration and OO::Monitors
machinery for a good while before the real cause — a clobbered topic, since
fixed in `news/2026-08/decl-time-value-block-keeps-the-topic.md` — turned up.

## Aside, measured on the way

mutsu's "Too many positionals passed" counts differently from raku's on a method
call: for `self.q(1, 2)` against `method q(Int $n)`, raku says "expected 2
arguments but got 3" (counting the invocant) and mutsu says "expected 1
arguments but got 2". Minor, and independent of the above.
