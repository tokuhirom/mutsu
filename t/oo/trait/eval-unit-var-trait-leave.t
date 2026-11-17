use Test;
use MONKEY-SEE-NO-EVAL;

# An EVAL'd unit runs the exit phasers a unit-level variable trait adds through
# `Variable.block.add_phaser`, and keeps its own tail value (#17202). The unit
# returns its tail on the VM stack; the phaser scope its LEAVE replay needs
# used to route the value through `$_`, so the unit answered whatever else the
# stack held (the traited variable after a `trait_mod:<does>` mixin).

plan 8;

my @log;
my $traits = q[
multi sub trait_mod:<is>(Variable:D $v, :$logged!) {
    $v.block.add_phaser("ENTER", { @log.push("enter") });
    $v.block.add_phaser("LEAVE", { @log.push("leave") });
}
];

is EVAL($traits ~ q[my $x is logged; @log.push("body"); 42]), 42,
    'the unit keeps its tail value';
is-deeply @log, ["enter", "body", "leave"], 'the trait LEAVE runs when the unit ends';

@log = ();
is EVAL($traits ~ q[my %h is logged = a => 1; %h<b> = 2; %h.elems]), 2,
    'a traited declaration with an initializer keeps the tail value';
is-deeply @log, ["enter", "leave"], 'and its ENTER and LEAVE run';

my $tagged = q[
my role Tagged { method tag { 'tagged' } }
multi sub trait_mod:<is>(Variable:D \v, :$tagged!) { trait_mod:<does>(v, Tagged) }
];
is EVAL($tagged ~ q[my %h is tagged = a => 1; %h.tag]), 'tagged',
    'a trait that mixes a role in keeps the tail value';
is EVAL($tagged ~ q[my %h is tagged = a => 1; 42]), 42, 'a literal tail too';
is EVAL(($tagged ~ q[my %h is tagged = a => 1; %h.tag]).AST), 'tagged',
    'EVAL of the tree likewise';

@log = ();
EVAL q[@log.push("a"); LEAVE { @log.push("l") }; 1];
is-deeply @log, ["a", "l"], 'a written LEAVE still runs';
