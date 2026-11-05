use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

plan 5;

# The builtin preludes (`trait_mod:<does>`, `trait_mod:<of>`, ...) are asked of
# the unit that runs. An EVAL'd string or tree never mentioned the name in the
# program's own text, so the unit ran without the prelude and died with
# "Unknown function: trait_mod:<does>" (#16203).

my constant $UNIT = q[
my role Tagged { method tag { 'tagged' } }
multi sub trait_mod:<is>(Variable:D \v, :$tagged!) { trait_mod:<does>(v, Tagged) }
my %h is tagged = a => 1;
%h.tag
];

is EVAL($UNIT), 'tagged', 'EVAL of a string: the unit gets trait_mod:<does>';
is EVAL($UNIT.AST), 'tagged', 'EVAL of a tree: the unit gets trait_mod:<does>';

# The program itself also mentions the name, so it has the prelude already:
# the unit's own copy must not clash with it.
my role Own { method tag { 'own' } }
multi sub trait_mod:<is>(Variable:D \v, :$own!) { trait_mod:<does>(v, Own) }
my %own is own = a => 1;
is %own.tag, 'own', 'the program has the prelude';
is EVAL($UNIT), 'tagged', 'a unit evaluated next to a program that has it too';

# A unit that declares its own handler keeps it.
is EVAL(q[sub trait_mod:<of>(|) { 'mine' }; trait_mod:<of>()]), 'mine',
    'a unit declaring trait_mod:<of> keeps its own';
