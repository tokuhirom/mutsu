use v6.d;
use Test;

plan 6;

# A variable trait handler may mix a role into the reflective `Variable`
# (Injector does this). The role belongs to the Variable object, not to the
# variable it reflects, and the object keeps answering .var/.name/.block.

role Tagged {
    method seed { self.block.add_phaser("ENTER", { self.var = 42 without self.var }) }
}
multi trait_mod:<is>(Variable:D $v, :$tagged!) {
    $v does Tagged;
    $v.seed;
}

my Int $c is tagged;
is $c, 42, 'a phaser added from a method of the mixed-in role seeds the variable';
is $c.^name, 'Int', 'the variable itself carries no mixin';

my @seen;
multi trait_mod:<is>(Variable:D $v, :$probe!) {
    $v does Tagged;
    @seen.push($v.var.^name, $v.name, $v.block.^name, $v.^name);
}
my Str $s is probe;
is-deeply @seen, ['Str', '$s', 'Block', 'Variable+{Tagged}'],
    '.var, .name and .block still answer on the mixed Variable';

sub f { my Int $x is tagged; $x }
is f(), 42, 'inside a routine too';
is f(), 42, '... on every call';
nok $c.can('seed'), 'the role did not leak into the variable';
