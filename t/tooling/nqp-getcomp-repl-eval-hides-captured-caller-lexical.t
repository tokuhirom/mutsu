use Test;
use nqp;

# A unit `Perl6::Compiler.eval` runs without an outer context sees the setting
# only, also a caller lexical a closure captured: the shared cell the unit
# holds for its callbacks (#16977) is not a declaration of the unit's code.
plan 4;

my $compiler = nqp::getcomp("Raku");
my $captured = 1;
my @captured-list = 1, 2;
my &reader = { $captured + @captured-list };
dies-ok { $compiler.eval('$captured') }, 'a captured caller scalar is not declared in the unit';
dies-ok { $compiler.eval('@captured-list') }, 'nor a captured caller array';
lives-ok { $compiler.eval('my $own = 3; $own + 1') }, 'the unit still declares its own lexicals';
is reader(), 3, 'the caller keeps its lexicals';
