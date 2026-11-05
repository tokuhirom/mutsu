use Test;

# A routine that hands back a `$` variable holding an Array/List/Hash (or a
# subclass or `but`/`does` mixin of one) is not sunk, while a fresh aggregate
# or an ordinary object held in a variable is (#16522). Expectations measured
# against rakudo 2026.09.

plan 11;

my $hits = 0;
my role Sinks { method sink { $hits++ } }
my class S { method sink { $hits++ } }
my class A is Array { method sink { $hits++ } }
my class L is List { method sink { $hits++ } }
my class H is Hash { method sink { $hits++ } }

sub foo3 { my $r = [<a b c>] does Sinks; $r }
sub g4 { my $r = [1, 2] but Sinks; $r }
sub g6 { my $r = (1, 2) but Sinks; $r }
sub g7 { my $r = {a => 1} but Sinks; $r }
$hits = 0; foo3; is $hits, 0, 'an Array mixin held in a variable is not sunk (does)';
$hits = 0; g4; is $hits, 0, 'an Array mixin held in a variable is not sunk (but)';
$hits = 0; g6; is $hits, 0, 'a List mixin held in a variable is not sunk';
$hits = 0; g7; is $hits, 0, 'a Hash mixin held in a variable is not sunk';

sub a1 { my $r = A.new(1, 2); $r }
sub l1 { my $r = L.new(1, 2); $r }
sub h1 { my $r = H.new; $r }
$hits = 0; a1; is $hits, 0, 'an Array subclass instance held in a variable is not sunk';
$hits = 0; l1; is $hits, 0, 'a List subclass instance held in a variable is not sunk';
$hits = 0; h1; is $hits, 0, 'a Hash subclass instance held in a variable is not sunk';

sub a3 { A.new(1, 2) }
sub foo { return [<a b c>] does Sinks }
sub g { my $r = S.new; $r }
sub g3 { my $r = S.new but Sinks; $r }
sub g5 { my $r = 5 but Sinks; $r }
$hits = 0; a3; is $hits, 1, 'a fresh Array subclass instance is sunk';
$hits = 0; foo; is $hits, 1, 'a returned fresh Array mixin is sunk';
$hits = 0; g; is $hits, 1, 'an object held in a variable is sunk';
$hits = 0; g3; g5; is $hits, 2, 'a scalar-valued mixin held in a variable is sunk';
