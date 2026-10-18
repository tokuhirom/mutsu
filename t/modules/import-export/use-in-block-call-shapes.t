use Test;

# A call of a routine a block-scoped `use` imported binds its positional
# arguments straight off the stack when it can (#13906); every other call
# shape takes the general entry. Each shape runs repeatedly, so the replayed
# import is exercised too. Checked against rakudo.
plan 9;

use lib 't/lib';

sub shapes($i) {
    use UseInBlockCallShapes;
    my @a = $i, 2;
    my $v = $i;
    uics-rw($v);
    (
        uics-add($i, 1),
        uics-add($i),
        uics-none(),
        uics-add(|@a),
        uics-named(:x($i)),
        uics-named(),
        $v,
        uics-all($i, 'a', 'b'),
        uics-list(@a),
    )
}

my @rows = (1..3).map({ shapes($_).list.join(',') });
is @rows[0], '2,11,none,3,2,2,3,1-a-b,2', 'every call shape, first call';
is @rows[2], '4,13,none,5,6,2,9,3-a-b,2', 'every call shape, later calls';

sub junction-of($i) { use UseInBlockCallShapes; uics-add($i | ($i + 1), 1) }
my $j = junction-of(1);
ok $j ~~ Junction, 'a Junction argument autothreads';
is $j.raku, 'any(2, 3)', 'over each eigenstate';
is junction-of(5).raku, 'any(6, 7)', 'on a later call too';

sub too-many($i) { use UseInBlockCallShapes; uics-none($i) }
dies-ok { too-many(1) }, 'an arity mismatch still dies';

sub in-loop() {
    use UseInBlockCallShapes;
    my $sum = 0;
    for ^4 { $sum += uics-add($_, 0) }
    $sum
}
is (^3).map({ in-loop() }).join(','), '6,6,6', 'calls in a loop of the block';

sub nested($i) { use UseInBlockCallShapes; uics-add(uics-add($i, 1), uics-add($i)) }
is (1..2).map({ nested($_) }).join(','), '13,15', 'nested calls';

sub rw-twice($x is copy) { use UseInBlockCallShapes; uics-rw($x); uics-rw($x); $x }
is (1..3).map({ rw-twice($_) }).join(','), '9,18,27', 'an is rw parameter writes the caller variable';
