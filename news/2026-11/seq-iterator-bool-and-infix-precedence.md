# Trait-less `infix:<...>` keeps its precedence; `.Bool` of an iterator Seq pulls one element

A `multi sub infix:<...>` without a precedence trait is a candidate of the core
sequence operator, so `1, 2 ... 4` still parses as `(1, 2) ... 4`; the operator
node dispatches to the declared candidate and falls back to the core body.
`Seq.new($iterator).Bool` pulls only the first element, and a following
`.head(n)` reads the pulled element and tops the Seq up instead of draining an
unbounded iterator (#16890).
