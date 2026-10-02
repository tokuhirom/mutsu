# Calling a candidate-less proto reports rakudo's "no candidates" error

`proto sub infix:<foo>($a, $b) {*}; say 1 foo 2` died with "Two terms in a
row": since #10516 the proto alone registers the operator for parsing, but the
`InfixFunc` fallback chain found nothing to call and reached its catch-all. And
`proto sub foo($a) {*}; foo(1)` said "none of these signatures matches" with an
empty signature list. Both now raise rakudo's `X::Multi::NoMatch`:

```
Cannot resolve caller infix:<foo>(Int:D, Int:D); Routine does not have any candidates.  Is only the proto defined?
```

`call_proto_dispatch` checks for registered candidates before choosing the
message, and the infix fallback asks `is_candidate_less_proto` (a `{*}`-only
proto with no candidate) before it gives up (mutsu#10531). A proto infix with a
body of its own is still not callable as an operator; that is #10696.
