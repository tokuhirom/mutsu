use Test;

# ADR-0113: a frame-lexical `my sub` is in no routine registry. A subscript
# argument (`f(@a[1])`) asks whether the callee binds that argument to the
# caller's container; the question must be answered from the chunk's own
# frame-lexical routine, as the call itself is. It used to fall through to a
# by-name resolution that found nothing -- so an `is rw` parameter rejected
# the element -- after walking every registered routine on every call
# (Digest::RIPEMD's `rotl($A[2], 10)`, #11686).

plan 8;

sub elements() {
    sub bump($x is rw) { $x++ }
    sub look($x) { $x * 10 }
    sub rawy(\x) { x = 7 }
    my @a = 1, 2, 3;
    bump(@a[1]);
    my $seen = look(@a[0]);
    rawy(@a[2]);
    my %h = a => 1;
    bump(%h<a>);
    (@a, $seen, %h<a>)
}
my ($a, $seen, $h) = elements();
is-deeply $a, [1, 3, 7], 'is rw and \\x parameters write through an array element';
is $seen, 10, 'a plain parameter reads the element';
is $h, 2, 'is rw writes through a hash element';

sub in-thread() {
    sub bump($x is rw) { $x++ }
    my @r = 10, 20;
    await start { bump(@r[0]) };
    @r
}
is-deeply in-thread(), [11, 20], 'the same call inside a start block';

sub readonly-arg() {
    sub set($x) { $x = 5 }
    my @a = 1;
    set(@a[0]);
    CATCH { default { return 'died' } }
    'lived'
}
is readonly-arg(), 'died', 'a plain parameter still rejects assignment';

sub in-loop() {
    sub rotl($n, $b) { (($n +< $b) +| ($n +> (32 - $b))) +& 0xFFFFFFFF }
    my @w = 1, 2, 3;
    my $s = 0;
    for ^100 { $s = rotl(@w[$_ % 3], 3) }
    $s
}
is in-loop(), 8, 'a frame-lexical call with a subscript argument in a loop';

sub shadowed() {
    sub pick($x is rw) { $x = 'lexical' }
    my @a = 'x';
    pick(@a[0]);
    @a[0]
}
sub pick($x) { 'package' }
is shadowed(), 'lexical', 'the frame-lexical routine wins over a same-named package sub';
is pick(1), 'package', 'the package sub is unaffected';
