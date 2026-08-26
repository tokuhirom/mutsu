# A grammar method called as `<.method>` gets an instance invocant

`Language/grammars.rakudoc` documents a grammar carrying its own methods and
calling them as assertions:

```raku
token a { \w+ <.mark> }
method mark(--> ::?CLASS:D) { $!invalid = True; self }
```

On mutsu the attribute write died with `Cannot look up attributes in a G type
object. Did you forget a '.new'?`, and even an attribute-free
`method mark() { self }` made the whole `G.parse("hello")` return `Nil`.

## Root cause

`try_regex_subrule_as_method` (`src/runtime/regex/regex_match_atom.rs`) called
the method with `Value::package(pkg)` — the grammar's *type object* — as the
invocant. A comment even recorded the compromise: it was enough for the one
shape the path was built for (`<.panic>`, a method that only dies), and full
cursor-self semantics were left as "a deeper feature". Two consequences
followed. Any attribute touch inside the method failed, because a type object
has no attribute storage. And the idiomatic `self` return was a type object,
which the return handling read as "no cursor" and therefore as a non-match, so
the enclosing token failed and the parse produced `Nil`.

A second, independent bug blocked the doc's exact spelling: a `--> ::?CLASS:D`
return constraint was stored verbatim. `::?CLASS` is fixed at compile time to
the declaring class — the parameter constraints were already substituted at
method registration, the return type was not — so the return check tried to
resolve a type literally named `::?CLASS:D` and the method's value came back
mangled.

## Fix

The assertion now builds an *instance* of the grammar carrying the cursor state
(`orig`, `from`, `pos`, `to`) and passes that as the invocant, so attribute
reads and writes work and method resolution still finds the grammar's own
method. A returned cursor of that grammar reports an absolute `pos`, which is
where the parse resumes — making the documented `{ …; self }` a zero-width
success at the current position. The existing Match-return path (and the
`<.panic>` die path) is unchanged.

`registration_class_body_method.rs` substitutes `::?CLASS` in a method's return
constraint exactly as it already did for its parameter constraints.

One divergence from raku remains and is tracked separately in
`todo/tickets/grammar-parse-result-is-a-match-not-a-grammar-cursor.md`: raku's
`G.parse(…)` returns a `G` cursor (`.WHAT` is `(G)`, `Grammar` being a `Match`
subclass), while mutsu returns a `Match`. That is a representation change far
outside this ticket; everything the ticket actually reported — the attribute
error and the `Nil` parse — is fixed.

Pin: `t/regex-embedded-code-blocks.t`.
