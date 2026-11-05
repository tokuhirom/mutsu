use v6;
use Test;

plan 6;

# Plain `&name` code variables are carried through the executing finalization
# (ADR-15760 slice 2) and still resolve routines, including later-declared ones.

sub twice($x) { $x * 2 }
my &alias = &twice;
is alias(4), 8, 'a code variable bound to a declared sub';
is &twice.name, 'twice', 'the routine keeps its name';
is (1, 2, 3).map(&twice).join(","), '2,4,6', 'a code variable as a callback';
is &alias.(5), 10, 'a code variable invoked with .()';
is &say.name, 'say', 'a core routine through its code variable';
sub later-user { &later-sub.() }
sub later-sub { 'late' }
is later-user(), 'late', 'a routine declared after its code variable use';
