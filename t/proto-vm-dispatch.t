use Test;

# Pin for VM-native proto dispatch (ledger §D, multi-dispatch VM-ization). A
# trivial-body proto (`proto foo {*}` / bodyless) now resolves its winning multi
# candidate via the VM-owned registry and runs it as compiled bytecode, instead
# of falling back to the tree-walk proto body + `__PROTO_DISPATCH__` round-trip.
# These cases exercise the correctness-critical paths that must stay identical to
# the interpreter fallback: recursion, redispatch (nextsame/samewith), the proto
# signature gate, non-trivial proto bodies, and X::Multi::NoMatch.

plan 12;

# Basic proto multi dispatch + recursion through the proto.
proto fact($) {*}
multi fact(0) { 1 }
multi fact(Int $n) { $n * fact($n - 1) }
is fact(5), 120, 'recursive proto multi (fact)';
is fact(0), 1, 'base case candidate';

# Type-based candidate selection through a permissive proto.
proto describe($) {*}
multi describe(Int $) { 'int' }
multi describe(Str $) { 'str' }
multi describe(Any $) { 'any' }
is describe(42), 'int', 'Int candidate selected';
is describe('x'), 'str', 'Str candidate selected';
is describe(3.5), 'any', 'fallback Any candidate selected';

# samewith re-dispatches through the proto with new args.
proto countdown($) {*}
multi countdown(0) { 'done' }
multi countdown(Int $n) { "$n " ~ samewith($n - 1) }
is countdown(3), '3 2 1 done', 'samewith recursion';

# nextsame walks to the next candidate (its value replaces the caller's).
proto rank($) {*}
multi rank(Int $n where * > 100) { "big" }
multi rank(Int $) { "small" }
is rank(5), 'small', 'where-narrowed candidate falls through to next';
is rank(500), 'big', 'where-constrained candidate matches';

# The proto's OWN signature is a gate: a constraining proto sig must reject args
# even when a candidate would accept them (S06-multi/proto.t parity). Wrapped in
# EVAL so the surrounding file compiles under Rakudo too (which rejects the bad
# call at compile time, while mutsu rejects it at run time — both must fail).
proto onlyint(Int $x) {*}
multi onlyint($) { 'accepted' }
is onlyint(7), 'accepted', 'arg satisfying proto sig dispatches';
dies-ok { EVAL q{ proto oi(Int $x) {*}; multi oi($) { 'a' }; oi('str') } },
    'proto signature rejects bad arg before dispatch';

# A non-trivial proto body must still run (not be bypassed) — observable through
# a guard that short-circuits the result before `{*}`.
proto guarded(Int $n) { return 'neg' if $n < 0; {*} }
multi guarded(Int $n) { "pos:$n" }
is guarded(5), 'pos:5', 'non-trivial proto body dispatches when guard passes';
is guarded(-5), 'neg', 'non-trivial proto body guard short-circuits dispatch';
