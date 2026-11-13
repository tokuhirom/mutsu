# A core operator's routine value runs the core operator

Calling `&[cmp]` as a value (`my &c = &[cmp]; c(1, 2)`, or a `:&cmp = &[cmp]`
parameter called as `cmp($a, $b)`) checked that no user candidate or `proto`
carries the name and then called the operator through the generic routine
path. That path checked for user candidates a second time and then looked up a
lexical `&infix:<cmp>` in the env of the scope that *called* the value, so a
closure declared there answered instead of the operator:

```raku
sub call(&c, $a, $b) { c($a, $b) }
my &plain = &[cmp];
{ my &infix:<cmp> = -> $, $ { "lexical" }; say call(&plain, 1, 2) }
```

raku says `Less`; mutsu said `lexical`. The value now runs the core operator
body once the user-candidate probes have declined. The second probe and the
env walk were also about a quarter of the cost of each `cmp` call in
Array::Sorted::Util's binary search (#15786).

Two related scoping bugs found on the way are filed as #17630 (a parameter
default `&[cmp]` sees the caller's lexical) and #17631 (a user `multi
infix:<cmp>` hides the core candidates from a routine value).
