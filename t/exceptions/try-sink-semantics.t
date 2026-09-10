use MONKEY-SEE-NO-EVAL;
use Test;
plan 14;

# A. try absorbs a trailing Failure value inside its protection (raku: P1/P2/Q1)
try { fail "x" };
ok $!.defined, 'try absorbs a literal trailing fail, $! set';
sub ff() { fail "x" }
try { ff() };
ok $!.defined, 'try absorbs a call-returned trailing Failure, $! set';
sub q1() { fail "x" }
my $r = try { q1() };
ok !$r.defined, 'absorbed-Failure try returns an undefined value';
is $!.^name, 'X::AdHoc', 'try sets $! to the exception behind the Failure';

# B. statement-position try's value IS sunk (side effects run) (raku: P6/P7)
my @forced;
sub seq-se() { (1..3).map({ @forced.push($_); $_ }) }
try { seq-se() };
is @forced.elems, 3, 'statement-position try forces a call-returned Seq';
@forced = ();
try { (1..3).map({ @forced.push($_); $_ }) };
is @forced.elems, 3, 'statement-position try forces a literal map Seq';

# C. explicit CATCH sees the force-time die (raku: P22/P23/Q8)
my $caught = False;
try { (1..3).map({ die "boom" }); CATCH { default { $caught = True } } };
ok $caught, 'explicit CATCH catches a force-time die of the tail map';
$caught = False;
sub die-seq() { (1..3).map({ die "boom" }) }
try { die-seq(); CATCH { default { $caught = True } } };
ok $caught, 'explicit CATCH catches a force-time die of a call-returned Seq';

# D. the eval_exception / eval-lives-ok cell (raku: P16/R4/R9) — this is the
#    exact Test.rakumod shape that costs advent2009-day20.t when broken.
sub eval_exception($code) {
    try { EVAL ($code); }
    $!
}
my $e = eval_exception(q[map -> $x, $y { ... }, 1..6]);
ok (not defined $e), 'eval_exception of a lazy stub map is not defined';
ok $e ~~ Failure, 'eval_exception of a lazy stub map returns a Failure';

# E. unit-scope escapes: sinking a lazy stub map at unit scope kills the
#    program even under try (raku parity: Q4/Q10/Q12/P17) — subprocess checks
my $p = run($*EXECUTABLE, '-e', 'use MONKEY-SEE-NO-EVAL; my $c = q[map -> $x, $y { ... }, 1..6]; try { EVAL $c; }; say "made it"', :out, :err);
ok $p.exitcode != 0, 'unit-scope try{EVAL lazy stub map} still dies at the statement sink (raku parity)';
unlike $p.out.slurp(:close), /'made it'/, '... and does not reach the next statement';
$p = run($*EXECUTABLE, '-e', 'sub f { map -> $x, $y { ... }, 1..6 }; try { f() }; say "made it"', :out, :err);
ok $p.exitcode != 0, 'unit-scope try{call returning lazy stub map} dies (raku parity)';
unlike $p.out.slurp(:close), /'made it'/, '... and does not reach the next statement';
