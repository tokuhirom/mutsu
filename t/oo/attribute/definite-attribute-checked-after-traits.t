use v6.d;
use Test;

# `has Int:D $.x` with no initializer is X::Syntax::Variable::MissingInitializer,
# but Rakudo checks AFTER the attribute's traits have run, and a trait may mark
# the attribute required (AttrX::Mooish does for `:lazy`). Inside a trait the
# attribute's `.type` is the definite type object.
use nqp;
plan 6;

my @probe;
multi trait_mod:<is>(Attribute:D $attr, :$probe!) {
    @probe.push: ($attr.type.^name, $attr.type.HOW.archetypes.definite.so);
}
multi trait_mod:<is>(Attribute:D $attr, :$requires!) {
    nqp::bindattr($attr, Attribute, '$!required', 1);
}
multi trait_mod:<is>(Attribute:D $attr, :$noop!) { }

class WithProbe { has Int:D $.a is probe is default(1); has Str $.b is probe; }
is-deeply @probe[0], ('Int:D', True), 'the type of an Int:D attribute is the definite type object';
is-deeply @probe[1], ('Str', False), 'a plain typed attribute is unchanged';

lives-ok { EVAL 'class Marked { has Str:D $.x is requires; }' },
    'a trait that marks the attribute required satisfies the check';

throws-like { EVAL 'class Unmarked { has Str:D $.x is noop; }' },
    X::Syntax::Variable::MissingInitializer,
    'a custom trait that does not mark it required still fails the check';

throws-like { EVAL 'class Bare { has Str:D $.x; }' },
    X::Syntax::Variable::MissingInitializer,
    'no trait: still a compile-time failure';

lives-ok { EVAL 'class Defaulted { has Str:D $.x is noop = "ok"; }' },
    'a written initializer satisfies the check';
