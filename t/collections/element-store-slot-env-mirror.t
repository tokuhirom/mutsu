use Test;

# An element store into a local the code declares operates on the slot
# (ADR-14632). A by-name reader that still exists (an `EVAL`) must see what
# the store wrote: the env mirror follows the slot.

plan 10;

sub slice-store() { my @a = 1, 2; @a[0, 1] = 7, 8; EVAL q[@a] }
is slice-store().join(','), '7,8', 'an EVAL sees a slice store into the routine\'s @a';

sub hash-slice-store() { my %h; %h<a b> = 1, 2; EVAL q[%h<b>] }
is hash-slice-store(), 2, 'an EVAL sees a hash slice store into the routine\'s %h';

sub nested-store() { my @a = [1], [2]; @a[1][0] = 9; EVAL q[@a[1][0]] }
is nested-store(), 9, 'an EVAL sees a nested store';

sub deep-store() { my @a = [[1],], [[2],]; @a[1][0][0] = 5; EVAL q[@a[1][0][0]] }
is deep-store(), 5, 'an EVAL sees a deep-nested store';

sub lazy-store() { my @l = 1..*; @l[2] = 99; (EVAL(q[@l[2]]), @l[0..3].join(',')) }
my ($seen, $prefix) = lazy-store();
is $seen, 99, 'an EVAL sees a store into a lazy array';
is $prefix, '1,2,99,4', 'the lazy array keeps its other elements and its tail';

sub shadowed() {
    my @a = 1, 2;
    if True { my @a = 3, 4; @a[0, 1] = 9, 9; }
    @a[0, 1] = 7, 8;
    @a
}
is shadowed().join(','), '7,8', 'a store after an inner same-named @a writes the outer one';

sub shadowed-hash() {
    my %h = a => 1;
    if True { my %h = b => 2; %h<x y> = 1, 2; }
    %h<p q> = 3, 4;
    %h.keys.sort.join(',')
}
is shadowed-hash(), 'a,p,q', 'a store after an inner same-named %h writes the outer one';

sub typed-slice() { my Int @a; @a[0, 1] = 1, 2; @a.of.^name }
is typed-slice(), 'Int', 'a slice store keeps the declared element type';

sub typed-refuses() {
    my Int @a;
    my $err = '';
    try { @a[0, 1] = 1, 'x'; CATCH { default { $err = .^name } } }
    $err
}
is typed-refuses(), 'X::TypeCheck::Assignment', 'a slice store still checks the declared type';
