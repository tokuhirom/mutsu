# `my %h = Any` is an odd hash initializer

Assigning a lone type object to a hash built `{(Any) => (Any)}`, a key taken
from the type object's gist and paired with a made-up `Any` value (#9774). A
type object is one element, and a one-element hash initializer is odd, so
`my %h = Any` and `%h = Int` now die with `X::Hash::Store::OddNumber` as the
`my %h = 1` case already did.

The error's message now uses rakudo's wording and shows the element the way
rakudo renders it:

```
Odd number of elements found where hash initializer expected:
Only saw: type object 'Any'
```

A longer list reads `Found 3 (implicit) elements:` / `Last element seen: "c"`,
with the element shown as its `.raku`. Before, the error said
`found 1 element(s); last element seen: (Any)` in mutsu's own format. The
list-assignment path and the `.Hash` / `.Map` coercions now build this error
through one shared function.
