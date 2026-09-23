use Test;

# ADR-0112: a `my sub` its enclosing routine only ever calls by bare name is
# bound as a frame lexical -- never installed in the routine registry, called
# straight through its compiled body. Everything below must behave exactly
# like an ordinary lexical sub.

plan 21;

# Shadowing a same-named outer sub, on both sides of the call.
sub shadow() { 'outer' }
sub uses-shadow() {
    my sub shadow() { 'inner' }
    shadow() ~ '/' ~ shadow()
}
is uses-shadow(), 'inner/inner', 'inner sub shadows the outer one';
is shadow(), 'outer', 'outer sub is untouched after the call';
is uses-shadow(), 'inner/inner', 'still shadows on a repeat call';

# Not visible outside its routine.
sub hides() { my sub only-inside() { 42 }; only-inside() }
is hides(), 42, 'callable inside';
throws-like { EVAL 'only-inside()' }, Exception, 'not callable outside';

# Recursion and sibling calls.
sub fact($n) {
    my sub f($k) { $k <= 1 ?? 1 !! $k * f($k - 1) }
    f($n)
}
is fact(6), 720, 'inner sub recurses into itself';
sub siblings($x) {
    my sub a($v) { b($v) + 1 }
    my sub b($v) { $v * 10 }
    a($x)
}
is siblings(3), 31, 'inner subs call each other';

# Captures of the enclosing routine's lexicals, read and written.
sub captures(int $pos) {
    my $count = 0;
    my sub bump() { $count++; $pos + $count }
    my $first = bump();
    my $second = bump();
    "$first $second $count"
}
is captures(10), '11 12 2', 'reads a parameter and writes an enclosing lexical';
is captures(20), '21 22 2', 'fresh enclosing state on every call';

# Called from a closure inside the routine.
sub from-closure($p) {
    my sub twice() { $p * 2 }
    (1..3).map({ twice() + $_ }).list
}
is from-closure(5), (11, 12, 13), 'called from a closure inside the routine';

# Argument shapes.
sub shapes() {
    my sub named(:$a = 1, :$b = 2) { "$a$b" }
    my sub slurpy(*@xs) { @xs.elems }
    my sub dflt($x, $y = 'd') { "$x$y" }
    named(b => 5) ~ ',' ~ slurpy(1, 2, 3) ~ ',' ~ dflt('x') ~ ',' ~ dflt('x', 'y')
}
is shapes(), '15,3,xd,xy', 'named, slurpy and defaulted parameters';

sub rw-param() {
    my $v = 1;
    my sub set-it($x is rw) { $x = 99 }
    set-it($v);
    $v
}
is rw-param(), 99, 'an is rw parameter writes through to the caller';

sub typed() {
    my sub wants-int(Int $x) { $x }
    my $s = 'nope';
    wants-int($s)
}
throws-like { typed() }, X::TypeCheck, 'parameter type check still applies';

sub junctions() {
    my sub inc(Int $x) { $x + 1 }
    so inc(1 | 2) == 3
}
ok junctions(), 'a Junction argument autothreads';

# Statement position (the value is sunk) and a Failure that is sunk.
sub statement-call() {
    my @log;
    my sub note-it($x) { @log.push($x) }
    note-it('a');
    note-it('b');
    @log.join
}
is statement-call(), 'ab', 'statement-position calls';
sub sunk-failure() {
    my sub fails() { fail 'boom' }
    fails();
    'not reached'
}
throws-like { sunk-failure() }, Exception, message => 'boom', 'a sunk Failure still throws';

# From a thread started inside the routine.
sub threaded($p) {
    my sub work() { $p + 1 }
    await start { work() }
}
is threaded(41), 42, 'called from a thread started in the routine';

# Identical inner subs in routines of different packages.
package PA { our sub go() { my sub tag() { $?PACKAGE.^name }; tag() } }
package PB { our sub go() { my sub tag() { $?PACKAGE.^name }; tag() } }
is PA::go(), 'PA', 'first package';
is PB::go(), 'PB', 'second package';

# Recursion into the enclosing routine re-enters the inner sub.
sub outer-rec($n) {
    my sub step() { $n <= 0 ?? 0 !! $n + outer-rec($n - 1) }
    step()
}
is outer-rec(4), 10, 'recursion through the enclosing routine';

# A die inside the inner sub names it in the backtrace.
sub dies-inside() {
    my sub boom() { die 'x' }
    boom()
}
{
    dies-inside();
    CATCH { default { like .backtrace.Str, /boom/, 'backtrace names the inner sub' } }
}
