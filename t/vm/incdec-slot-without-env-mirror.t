use Test;

# A `++`/`--` on a slotted local no longer forces that local's env mirror to
# be kept current: the op reads and writes the slot, including on its slow
# path (a Str, a Rat, an overflow, an undefined start). Every shape that
# leaves the Int fast path must still see the value the slot holds.
plan 10;

sub f() {
    my int $p = 0;
    $p++ while $p < 5;
    $p;
}
is f(), 5, 'native int counter in a statement-modifier loop';

sub g() {
    my $s = 'a';
    $s++;
    $s = 'y';
    $s++;
    $s;
}
is g(), 'z', 'a Str stepped after an assignment reads the assigned value';

sub h() {
    my $n = 1;
    $n++;
    $n = 'Az';
    ++$n;
}
is h(), 'Ba', 'prefix step of a Str assigned after an Int step';

sub big() {
    my $b = 9223372036854775806;
    $b++;
    $b++;
    $b++;
    $b;
}
is big(), 9223372036854775809, 'overflow past int64 continues from the slot';

sub rat() {
    my $r = 1;
    $r++;
    $r = 2.5;
    $r--;
    $r;
}
is rat(), 1.5, 'a Rat assigned after an Int step';

sub undef-start() {
    my $u;
    $u++;
    $u++;
    $u;
}
is undef-start(), 2, 'an undefined start counts from zero';

sub closure-reads() {
    my $c = 0;
    my &peek = { $c };
    $c++;
    $c++;
    peek();
}
is closure-reads(), 2, 'a closure sees the stepped value';

sub closure-writes() {
    my $c = 0;
    my &bump = { $c++ };
    bump() for ^3;
    $c++;
    $c;
}
is closure-writes(), 4, 'steps by a closure and by the frame share one variable';

my $top = 0;
$top++ for ^4;
is $top, 4, 'a file-scope counter';

sub eval-sees() {
    my $e = 10;
    $e++;
    EVAL '$e';
}
is eval-sees(), 11, 'EVAL reads the stepped value by name';
