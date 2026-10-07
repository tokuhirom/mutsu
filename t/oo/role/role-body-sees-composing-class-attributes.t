use Test;
use lib 't/lib';
use RoleBodyLexicalRole;

# From the ValueType distribution: a role body is run per composing class and
# its `::?CLASS.^attributes` lists the class's own `has` declarations, with any
# attribute traits already applied.

plan 5;

role Plain {
    has $!own;
    my @seen = ::?CLASS.^attributes.map(*.name);
    method seen { @seen.List }
}
class A does Plain { has $.x; has $.y; }
class B does Plain { has $.z is rw; }
is-deeply A.new.seen, ('$!x', '$!y'), 'role body lists the class attributes (A)';
is-deeply B.new.seen, ('$!z',), 'each composing class gets its own list (B)';

class C does Counted {
    has $.keep;
    has $!skip is hidden-here;
}
is-deeply C.new.counted-names, ('$!keep',),
    'a role body in another file sees its module lexical role and the trait effect';
is-deeply C.^attributes.map(*.name).List, ('$!keep', '$!skip'),
    'the stand-in attributes do not leak into the class';
is C.^attributes.elems, 2, 'no duplicate attributes after composition';

# vim: expandtab shiftwidth=4
