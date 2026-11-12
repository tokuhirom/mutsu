# A by-name `%h := $hash` bind shares the Hash instead of copying it

A `%`-sigil bind that reaches its variable by name rather than through a local
slot of the binding frame (an `our %h` bound inside a sub, or a closure built
from RakuAST whose `%h` lives in an outer routine) went through the assignment
coercion, which takes a snapshot of a shared hash node. The bound name then held
a copy: later writes through the source never showed up through `%h`, and vice
versa once either side was written.

The by-name store now binds the Hash itself, as the slot-addressed store already
did, after the same Associative and value-type checks. This fixes EEL's fork
restore (`%state := $_` in a slang-built block), where `:my %state` and a
`:my $alias = %state` alias drifted apart in each fork: `t/07-goal.t` #67,
`t/24-interleave.t` #35 and `t/29-optimizations.t` #26 now match rakudo (#17494).
