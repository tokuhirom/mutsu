use Test;
plan 8;

# A statement call to a multi sinks the fresh value it hands back, as a call to
# a plain sub does (perl-func.rakudoc's `increment`). Expected counts are
# rakudo 2026.09's.
my @log;
class A { method sink { @log.push: 'sunk' } }
my $keep = A.new;

multi m1($x) { A.new }
m1(1);
is @log.elems, 1, 'a multi call result runs its sink';

multi m2($x) { A.new }
multi m2($x, $y) { A.new }
@log = ();
m2(1); m2(1, 2);
is @log.elems, 2, 'every candidate of a multi sinks';

multi m3(Int $x) { A.new }
multi m3(Str $x) { A.new }
@log = ();
m3("a"); m3(2);
is @log.elems, 2, 'candidates split by type sink';

multi m4($x) { return A.new }
@log = ();
m4(1);
is @log.elems, 1, 'a returned fresh value is sunk';

multi m5($x) is rw { $keep }
@log = ();
m5(1);
is @log.elems, 0, 'an `is rw` multi hands back a container: no sink';

proto m6($) {*}
multi m6($x) { A.new }
@log = ();
m6(1);
is @log.elems, 1, 'a multi behind a proto sinks';

multi m7($x) { 5 does role { method sink { @log.push: 'role-sink' } } }
@log = ();
m7(1);
is @log.join(','), 'role-sink', 'a mixin result runs its composed sink';

sub s1($x) { A.new }
@log = ();
s1(1);
is @log.elems, 1, 'a plain sub still sinks (unchanged)';
