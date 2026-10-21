use Test;
use nqp;

plan 9;

# `Attribute.get_value($obj)` hands out the attribute's Scalar container, so
# `nqp::iscont` holds and a store through it is the attribute's own
# (#14266, AttrX::Mooish). An absent slot still answers a plain `Nil`, which the
# lazy-initialization idiom (`get_value // set_value`) relies on.
class C {
    has Int $.i = 3;
    has $.u;
    has Stringy $.s = Str;
}
class Z { has $.x; }
my $c = C.new;
my ($ai, $au, $as) = C.^attributes;

ok nqp::iscont($ai.get_value($c)), 'a typed attribute answers its container';
ok nqp::iscont($au.get_value($c)), 'an unset attribute answers its container';
ok nqp::iscont($as.get_value($c)), 'an attribute holding a type object answers its container';

my $v := $ai.get_value($c);
$v = 9;
is $c.i, 9, 'a store through the bound container is the attribute';
is $ai.get_value($c), 9, 'and reads back';

my $old = nqp::clone($ai.get_value($c));
$v = 10;
is $old, 9, 'nqp::clone of the value is a copy, not the container';

my Mu $aviv := Z.^attributes[0].auto_viv_container;
ok nqp::iscont($aviv), 'auto_viv_container is a container';
ok nqp::isconcrete_nd($aviv), 'and concrete';
my $fresh = C.new;
is-deeply $ai.get_value($fresh).VAR.^name, 'Scalar', 'a fresh instance too';
