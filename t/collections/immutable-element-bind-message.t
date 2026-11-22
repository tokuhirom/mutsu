use Test;

# Binding an element of an immutable container: rakudo's `Map.BIND-KEY` names
# the type ("Cannot bind to Map"), and a List or Range has no BIND-POS slot at
# all, so the bind dies with a target-less X::Bind ("Cannot use bind operator
# with this left-hand side"). Neither names the variable.

plan 6;

my $m = Map.new((a => 1));
throws-like { $m<z> := 5 }, X::Bind, message => 'Cannot bind to Map', 'a new key of a Map';
throws-like { $m<a> := 5 }, X::Bind, message => 'Cannot bind to Map', 'an existing key of a Map';
my %b := Map.new((a => 1));
throws-like { %b<a> := 5 }, X::Bind, message => 'Cannot bind to Map', 'a Map bound to a % variable';

my $l = (1, 2);
throws-like { $l[0] := 3 }, X::Bind,
    message => 'Cannot use bind operator with this left-hand side', 'a List element';
my $r = 1..3;
throws-like { $r[0] := 3 }, X::Bind,
    message => 'Cannot use bind operator with this left-hand side', 'a Range element';
my @r := 1..3;
throws-like { @r[0] := 3 }, X::Bind,
    message => 'Cannot use bind operator with this left-hand side', 'a Range bound to an @ variable';
