use Test;

# ADR-12529 phase 3, slice 3: a named routine declared at the top of the
# program that looks names up reflectively (EVAL, ::('$x')) resolves a lexical
# through its own frame and the program scope, never through its callers.
# Dynamic variables still resolve through the callers. Every expectation was
# checked against rakudo.

plan 10;

my $p = 'program';

sub own() { my $mine = 'own'; EVAL q[$mine] }
is own(), 'own', 'EVAL sees the routine\'s own lexical';

sub write-own() { my $w = 1; EVAL q[$w = 2]; $w }
is write-own(), 2, 'EVAL writes the routine\'s own lexical';

sub read-p() { EVAL q[$p] }
sub shadow-p() { my $p = 'caller'; read-p() }
is shadow-p(), 'program', 'EVAL reads the program-scope lexical, not a caller\'s same-named one';

sub sym-p() { ::('$p') }
sub shadow-sym() { my $p = 'caller'; sym-p() }
is shadow-sym(), 'program', '::(\'$x\') reads the program-scope lexical, not a caller\'s';

sub sym-hidden() { (try ::('$only-caller')) // 'not visible' }
sub call-sym() { my $only-caller = 'caller'; sym-hidden() }
is call-sym(), 'not visible', '::(\'$x\') does not find a lexical only a caller declares';

sub dyn() { EVAL q[$*reflective-dyn] }
sub set-dyn() { my $*reflective-dyn = 'dynamic'; dyn() }
is set-dyn(), 'dynamic', 'EVAL still finds a dynamic variable through the callers';

sub deep-eval($n) {
    $n == 0 ?? (try EVAL q[$deep-local]) // 'not visible'
            !! do { my $deep-local = 'caller'; deep-eval($n - 1) }
}
is deep-eval(3), 'not visible', 'a recursive routine\'s EVAL does not see its outer activations\' lexicals';

sub outer-routine() {
    my $x = 'outer';
    sub inner-routine() { EVAL q[$x] }
    inner-routine()
}
is outer-routine(), 'outer', 'a sub declared in a routine still sees that routine\'s lexical';

sub with-context($code) { my $ctx = CALLER::; EVAL $code, context => $ctx }
sub ctx-caller() { my $cv = 42; my &keep = { $cv }; with-context(q[$cv + 1]) }
is ctx-caller(), 43, 'EVAL with context => CALLER:: sees the caller\'s lexical';

sub callee-of-eval() { (try EVAL q[$between]) // 'not visible' }
sub middle() { my $between = 'middle'; EVAL q[callee-of-eval()] }
is middle(), 'not visible', 'a routine called from EVAL does not see the EVAL caller\'s lexical';

done-testing;
