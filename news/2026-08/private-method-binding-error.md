# A private method that fails to bind reports the binding error

```raku
class C {
    method !p(Int $n) { $n * 2 }
    method go() { self!p("not-an-int") }
}
C.new.go;
```

```
before:  No such private method 'p' for invocant of type 'C'
after:   Type check failed in binding to parameter 'n'; expected Int, got Str
```

Same for arity — `self!p(1, 2)` said "No such private method" and now says
"Too many positionals passed". The **public** forms of both were already right,
so this was purely the `!`-private path.

## Root cause

`resolve_private_method_any_owner` / `resolve_private_method_with_owner` pick a
candidate by matching the arguments against each overload's signature. `None`
therefore meant two different things — "the class has no such private method"
and "it has exactly one, whose parameters these arguments do not bind" — and the
caller reported the first.

## Fix

`private_method_candidates_by_name` finds candidates by name only. When the
signature-matching resolvers come back empty, the dispatch site consults it:

- no candidate → the not-found error stands;
- exactly one → run it, so the binding machinery produces raku's error;
- more than one → `X::Multi::NoMatch`.

The private-attribute form (`self!Owner::attr`, resolved after the method lookup
fails) is unaffected — it only applies when no method of that name exists.

Pinned by `t/private-method-binding-error.t`.

## Why it was worth doing

It was actively misleading, not cosmetic. Cro's `CookieJar.add-from-response`
calls `self!get-cookie-lifetime($_, $state)` in a loop; a separate bug left `$_`
holding a `Bool` instead of a `Cro::HTTP::Cookie`, and the signature mismatch
surfaced as `No such private method 'get-cookie-lifetime' for invocant of type
'Cro::HTTP::Client::CookieJar'`. That sent the investigation into method
registration and OO::Monitors before the real cause — a clobbered topic, fixed
in `news/2026-08/decl-time-value-block-keeps-the-topic.md` — turned up.
