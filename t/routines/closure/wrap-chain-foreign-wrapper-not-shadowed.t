use Test;

plan 1;

# ADR-0019 E9b-2 (P1): a wrapped method called from inside a FOREIGN
# wrapper's dispatch loses its own wrap chain, because a global
# `is_inside_wrap_dispatch()` guard suppressed EVERY wrap chain while ANY
# wrap dispatch was live, not just the one it was meant to protect. Verified
# against Rakudo v2026.06 (`raku`): the expected output is
# 'x-wrap[x-orig]+y-wrap[y-orig]' -- both wrap chains fire. Before the E9b-2
# fix, mutsu printed 'x-wrap[x-orig]+y-orig': B's wrap chain was silently
# skipped because A's own wrap dispatch was still active on the stack.
class A { method x() { "x-orig" } }
class B { method y() { "y-orig" } }
A.^lookup('x').wrap(-> $self { "x-wrap[" ~ callsame() ~ "]+" ~ B.new.y });
B.^lookup('y').wrap(-> $self { "y-wrap[" ~ callsame() ~ "]" });

is A.new.x, 'x-wrap[x-orig]+y-wrap[y-orig]',
    'a wrapped method called from inside a foreign wrapper still enters its own wrap chain';
