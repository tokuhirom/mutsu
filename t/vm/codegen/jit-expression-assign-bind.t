use Test;
use nqp;

# Routines with an expression-form local assignment (`($i = ...)`) and a
# `my $x := ...` bind, called often enough to run as native JIT bodies,
# answer exactly what the interpreter answers on their first calls.

plan 7;

sub count-down(int $n) {
    my int $i = $n;
    my int $hits;
    nqp::while(
      nqp::isge_i(($i = nqp::sub_i($i, 1)), 0),
      ($hits = nqp::add_i($hits, 1))
    );
    $hits
}
is-deeply (^300).map({ count-down($_ % 7) }).sum, (^300).map({ $_ % 7 }).sum,
    'nqp::while with expression assignments';

sub bound(@a, $k) {
    my $v := @a[$k];
    my $w = ($v // 0) + 1;
    ($w = $w * 2)
}
my @a = 1, 2, 3;
is-deeply (^300).map({ bound(@a, $_ % 4) }).List.head(8),
    (4, 6, 8, 2, 4, 6, 8, 2), 'scalar binds and an expression assignment as the value';

sub bind-ro($x) {
    my $y := $x;
    $y = 5;
}
my @errs = (^150).map({ (try { bind-ro($_); 'no' }) // $!.^name });
ok @errs.unique.elems == 1 && @errs[0] ne 'no', 'assigning a := bound readonly value dies every time';

sub state-count() {
    state $n = 0;
    ($n = $n + 1)
}
state-count() for ^200;
is state-count(), 201, 'an expression assignment to a state variable persists';

sub typed(Int $x) {
    my Int $t;
    ($t = $x)
}
is (^200).map({ typed($_) }).sum, (^200).sum, 'typed local, values that pass';
dies-ok { typed-str() for ^200 }, 'a type-check failure in an expression assignment dies';
sub typed-str() { my Int $t; ($t = "x") }

sub swap(Int $a is copy, Int $b is copy) {
    ($a, $b) = ($b, $a);
    my $sum := $a + $b;
    ($a = $a - $b) + $sum
}
is (^200).map({ swap($_, 1) }).sum, (^200).map({ 1 - $_ + 1 + $_ }).sum,
    'list assignment, a bind and an expression assignment together';
