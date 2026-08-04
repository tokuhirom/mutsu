# A pair read out of a Hash is a positional argument

In Raku, whether an argument is named is a property of the **call site**, not of
the value: only a literal `k => v` / `:k(...)` written in the argument list is a
named argument. A `Pair` that arrives through a variable — including one read
out of a Hash — is an ordinary positional argument.

mutsu encodes named-ness in the value instead (`ValueRepr::Pair` with a `Str`
key means "named argument", `ValueRepr::ValuePair` means "positional"), so every
hash-derived pair was misread as a named argument:

```raku
class C {
    multi method take(Pair $p) { "Pair" }
    multi method take(Str $s)  { "Str"  }
}
my %h = a => 1;
C.new.take(%h.pairs[0]);    # Cannot resolve caller take(C:D: :a(Int))
```

`%h.pairs`, `%h.List`, `%h.antipairs`, `%h.invert` and iterating `%h` now all
build the positional flavour, so each of those binds the `Pair` candidate as
`raku` does.

Two consequences fell out of it:

- **`-> (:$k)` is a destructure, not a named parameter.** The lambda parser
  flattened a lone named sub-signature parameter to the top level, which only
  appeared to work while the pair being destructured was itself a named
  argument. It now becomes a `__subsig__` destructuring parameter, like every
  other parenthesised signature. (`-> :$k` remains the named parameter.)
- **A `ValuePair` destructures exactly like a `Pair`.**
  `named_values_from_unpack_target` handled only the latter; the two differ only
  in what the call site meant, which has nothing to do with `Pair.Capture`.

`t/coercion-type-regressions.t` was pinning `-> (:Str(Any) :$suffix)`, which
`raku` rejects outright ("Missing block"); it now uses the shape the
authoritative roast test (`S12-coercion/coercion-types.t`, rakudo #1800) writes,
`-> % ( Str(Any) :$suffix )`.

This is one slice of a wider problem — `Pair.new`, a fat-arrow assigned to a
variable, and a fat-arrow inside an array literal still mint the named flavour.
The remainder is tracked in
`todo/deep/pair-namedness-is-a-value-property-not-a-call-site-property.md`.

With this and `%h.List` yielding pairs, `Cro::HTTP::Client` accepts
`headers => %h`, so a request carrying custom headers completes: the vendored
Cro suite's `http-middleware.rakutest` subtest 3 ("Conditional response
middleware") now passes in full, as does subtest 7.

Pinned by `t/hash-pair-is-positional-argument.t`.
