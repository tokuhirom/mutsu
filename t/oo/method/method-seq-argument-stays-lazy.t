use Test;

# A `.map` Seq passed to a user-declared method is bound lazily, as it is to a
# sub: the method pulls only what it reads. The argument boundary of a method
# call forced the whole `.map` first, so a feeder that pulls a 200 000-row
# generator chunk by chunk ran it to the end before writing its first chunk
# (Rakuish t/48-exec-stdin.t `Stream.exec`).

plan 9;

my $pulled = 0;
sub mk() { (1 .. 20).map(-> $i { $pulled++; $i }) }

class S {
    method take3($rows) { my $it = $rows.iterator; $it.pull-one for ^3; }
    method !priv3($rows) { my $it = $rows.iterator; $it.pull-one for ^3; }
    method via-private() { self!priv3(mk()) }
    method gather3($rows) {
        my $g = gather { my $it = $rows.iterator; loop { my $r = $it.pull-one; last if $r ~~ IterationEnd; take $r } };
        my $it = $g.iterator; $it.pull-one for ^3;
    }
    method slurpy(*@a) { @a.elems }
    method sum(*@a) { [+] @a }
    method positional(@a) { @a.elems }
}

$pulled = 0; S.take3(mk());         is $pulled, 3, 'type-object invocant pulls what the method reads';
my $s = S.new;
$pulled = 0; $s.take3(mk());        is $pulled, 3, 'instance in a variable';
$pulled = 0; S.new.take3(mk());     is $pulled, 3, 'chained instance';
$pulled = 0; $s.via-private;        is $pulled, 3, 'private method';
$pulled = 0; $s.gather3(mk());      is $pulled, 3, 'a gather over the argument stays lazy';

is $s.slurpy((1..3).map(* * 2)), 3, 'a slurpy parameter still flattens the Seq';
is $s.sum((1..3).map(* * 2)), 12, 'and reads its elements';
is $s.positional((1..3).map(* * 2)), 3, 'an @-parameter binds it';

my @a = 1, 2, 3, 4;
@a.splice(1, 2, (7, 8).map(* * 10));
is-deeply @a, [1, 70, 80, 4], 'a native method still reads a .map argument';
