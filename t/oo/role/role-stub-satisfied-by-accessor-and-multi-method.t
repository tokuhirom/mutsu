use v6.d;
use Test;

# From PDF::Font::Loader (PDF::Font::Loader::Enc::Type1): the parent class
# declares both `has %.charset` and explicit `multi method charset` candidates,
# and the subclass composes a role whose stub is `method charset {...}`. The
# explicit method replaces the generated accessor, so there is one candidate,
# not two ("multiple inherited candidates").

plan 5;

role Encoder { method charset {...} }

class Base {
    has %.charset;
    multi method charset($k) { %!charset{$k} }
    multi method charset { %!charset }
}
class Other { method other { 1 } }

lives-ok { EVAL 'class Child is Base is Other does Encoder { }' }, 'two parents, accessor plus multis';
my class Kid is Base does Encoder { }
is-deeply Kid.new.charset, {}, 'nullary candidate answers';
is Kid.new(charset => {1 => 2}).charset(1), 2, 'keyed candidate answers';

# A plain accessor alone still satisfies the stub.
class OnlyAccessor does Encoder { has %.charset }
is-deeply OnlyAccessor.new.charset, {}, 'accessor alone satisfies the stub';

# An unimplemented stub is still rejected.
throws-like { EVAL 'class Bad does Encoder { }' }, Exception, 'unimplemented stub still an error';

done-testing;
