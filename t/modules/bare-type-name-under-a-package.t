use Test;

# A class or role declared under a non-GLOBAL package registers
# package-qualified (`M::C`) -- which is what raku does too. Type-object
# position already resolved a bare reference to it by walking outward through
# the enclosing packages; *call* position and `augment` did not, and reported
# "Unknown function: C" / X::Augment::NoSuchType instead.

plan 7;

module M {
    class C {
        has $.v;
        method new($v) { self.bless(:$v) }
    }
    role R { has $.x }
    our sub coerce-it()   { C("x") }
    our sub role-init()   { 99 but R("ok") }
    our sub augment-it()  { EVAL 'use MONKEY-TYPING; augment class C { method extra { 7 } }' }
}

is M::coerce-it().v, 'x',
    'a bare class name coerces from inside its own package';

is M::role-init().x, 'ok',
    'a bare role name initializes its single public attribute';

is M::C.^name, 'M::C',
    'the declaration really is package-qualified';

# `augment` resolves the same way.
use MONKEY-SEE-NO-EVAL;
lives-ok { M::augment-it() },
    'augment finds the package-qualified class by its bare name';
is M::C.new('y').extra, 7,
    'and the augmented method is there';

# A genuinely absent type is still X::Augment::NoSuchType.
throws-like 'use MONKEY-TYPING; augment class NoSuchClassAtAll { }',
    X::Augment::NoSuchType,
    'an absent class still reports NoSuchType';

# The GLOBAL case is unchanged.
class Top { has $.v; method new($v) { self.bless(:$v) } }
is Top("z").v, 'z', 'a GLOBAL class still coerces by its bare name';
