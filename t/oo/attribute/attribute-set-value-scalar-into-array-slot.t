use Test;
use nqp;

# `Attribute.set_value` binds what it is given (`Mu \value`): a `$`-variable
# holding an Array binds a Scalar into an `@` attribute, so `get_value` hands
# out that container and `nqp::assign` through it stores into the slot. A
# clone keeps a Scalar of its own there (EEL's state copier, #17418).

plan 8;

class Pl { has @.pos; has %.h }
my $pos = Pl.^attributes.first(*.name eq '@!pos');
my $h = Pl.^attributes.first(*.name eq '%!h');

my $p = Pl.new;
my $copied = [1, 2];
$pos.set_value($p, $copied);
my $slot := $pos.get_value($p);
ok nqp::iscont($slot), 'the slot is a container';
nqp::assign($slot, [5]);
is-deeply $p.pos, [5], 'nqp::assign through it stores into the slot';

my $c = $p.Mu::clone;
my $cslot := $pos.get_value($c);
ok nqp::iscont($cslot), 'a clone has a container there too';
nqp::assign($cslot, [7]);
is-deeply $c.pos, [7], 'assigning through it changes the clone';
is-deeply $p.pos, [5], 'and not the original';

my $q = Pl.new(:pos(1, 2));
nok nqp::iscont($pos.get_value($q)), 'an ordinary @ slot is not a container';

my $hv = {a => 1};
$h.set_value($p, $hv);
my $hslot := $h.get_value($p);
nqp::assign($hslot, {b => 2});
is-deeply $p.h, {b => 2}, 'the same for a % attribute';
is-deeply Pl.new(:h(x => 1)).h, {x => 1}, 'an ordinary % attribute is unchanged';
