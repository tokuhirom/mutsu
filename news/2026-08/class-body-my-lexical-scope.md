# A class-body `my` is a lexical of that body, not a global

`class C { my $x = ...; method m { $x } }` declares `$x` in the class body's
lexical scope. mutsu bound it under its bare name in the *enclosing* env and
left it there, which made every class-body static a de facto global: the next
class body declaring the same name simply overwrote it.

Worse, the damage was silent and retroactive. `inject_class_body_statics` — the
mechanism that makes a method see its own class's statics — only filled names the
method env did **not** already have, precisely so a parameter would take
precedence. With the leaked binding sitting in the env, that filter fired on it,
so the *first* class's methods read the *second* class's value:

```raku
class A { my constant @defaults = <a b>;   method get() { @defaults } }
class B { my constant @defaults = <x y z>; method get() { @defaults } }
say A.get;   # raku: (a b)   mutsu was: (x y z)
```

Cro declares four `my constant @defaults`, one per body-parser and
body-serializer selector class. The last one loaded won, so every *body parser*
lookup walked the *serializer* list and died with "Too few positionals passed;
expected 2 arguments but got 1" — `Cro::BodySerializer.is-applicable` takes a
message and a body, `Cro::BodyParser.is-applicable` only a message. Nothing in
Cro could decode a response body.

## The fix

The authoritative copy of a class-body static already lives in
`package_lexicals[C]`, recorded when the body finishes. Two changes make that
store load-bearing rather than a mirror:

- The bare env binding is restored to whatever the enclosing scope had (or
  removed) when the class body ends — the same treatment
  `news/2026-08/class-body-type-scope.md` gave nested type names. Only names the
  body genuinely `my`-declared are unbound: the recorded set deliberately
  over-approximates, and for a `unit class` it also picks up everything the
  body's own `use` statements imported. Unbinding *those* broke HTTP::UserAgent,
  whose `unit class HTTP::UserAgent; use HTTP::UserAgent::Common;` supplies the
  `%useragents` that the exported `get-ua` reads (caught by the batteries gate,
  not by `make test`).
- `inject_class_body_statics` now injects unconditionally, and both method
  dispatch paths call it *before* `self` and the parameters are bound. A
  parameter of the same name still wins — it simply overwrites — while a caller
  lexical that merely shares the name no longer shadows the class's own lexical.
  The method body is inside the statics' scope; the calling frame's binding is
  visible there only because mutsu's env is flattened.

## Effect

`t/http-response-parser.rakutest` and `t/http-request-parser.rakutest` now get
past body decoding entirely: the request parser's plan grew from 295 to 306 as
whole groups of previously-dead assertions (`.hash gives back Hash with 3
elements`, `Can index associatively`, the `application/x-www-form-urlencoded`
and JSON body suites) started running.

Pinned by `t/class-body-my-lexical-scope.t`, verified against `raku`.
