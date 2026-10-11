use Test;

# ADR-12529 phase 3, slice 3: a named routine declared at the top of the
# program that looks names up reflectively (EVAL, ::('$x')) resolves a lexical
# through its own frame and the program scope, never through its callers.
# Dynamic variables still resolve through the callers. Every expectation was
# checked against rakudo.

plan 16;

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

# --- closures (slice 4) ----------------------------------------------------

my $late = 1;
my &read-late = -> { EVAL q[$late] };
$late = 2;
is read-late(), 2, 'a closure\'s EVAL reads the program-scope lexical\'s current value';

sub call-closure(&f) { my $late = 'caller'; f() }
is call-closure(&read-late), 2, 'a program-scope closure\'s EVAL does not see its caller\'s lexical';

sub make-reader() { my $made = 'made'; -> { EVAL q[$made] } }
sub call-reader() { my $made = 'caller'; make-reader()() }
is call-reader(), 'made', 'a closure made in a routine reads that routine\'s lexical through EVAL';

my &outer-maker = -> { -> { (try EVAL q[$callers-only]) // 'not visible' } };
sub call-maker() { my $callers-only = 'caller'; outer-maker()() }
is call-maker(), 'not visible', 'a closure made in a closure does not capture its maker\'s caller';

my &sym-closure = -> { (try ::('$sym-caller')) // 'not visible' };
sub call-sym-closure() { my $sym-caller = 'caller'; sym-closure() }
is call-sym-closure(), 'not visible', 'a program-scope closure\'s ::(\'$x\') does not see its caller\'s lexical';

sub later-write() { my $lw = 1; my &c = -> { EVAL q[$lw] }; $lw = 2; c() }
is later-write(), 2, 'a closure made in a routine reads that routine\'s later write through EVAL';

done-testing;
