use nqp;

# Method calls on immutable scalars from TRIR routines (`MethodGen` sites):
# native methods answered by the scalar lane, a Failure-returning `.Numeric`,
# a junction argument (autothreaded by the general dispatch) and `.raku` of
# a scalar.

my sub numeric(str $s) { nqp::substr($s, 0, 4).Numeric }
my sub upper(str $s) { nqp::substr($s, 0, 5).uc }
my sub count(str $s) { nqp::substr($s, 0, 5).chars }
my sub where(str $s) { nqp::substr($s, 0, 5).index('c') }
my sub junction(str $s) { nqp::substr($s, 0, 3).contains('a' | 'z') }
my sub rendered(int $n) { nqp::add_i($n, 1).raku }

say 'numeric=', numeric('12.5x'), ' ', numeric('1e3'), ' ', numeric('3.25');
my $bad = numeric('abc');
say 'failure=', $bad.WHAT.^name, ' handled=', $bad.defined.not;
say 'methods=', upper('abcab'), ' ', count('abcab'), ' ', where('abcab');
say 'junction=', so(junction('abc')), ' ', so(junction('xyz'));
say 'rendered=', rendered(41);
