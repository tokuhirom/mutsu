use Test;

plan 6;

# `X.accessor<k>:exists` where the accessor is `is rw` and answers a Proxy (a
# column object in Red): the subscripted object is what the Proxy FETCHes, and
# its EXISTS-KEY answers. The Proxy itself was probed, so every key was absent.

my @asked;
class Answer {
    has $.truth;
    method Bool { @asked.push: "Bool"; $!truth }
}
class Col {
    method EXISTS-KEY(\k) { Answer.new(truth => k eq "yes") }
}
class Model {
    has $.col = Col.new;
    method col is rw { Proxy.new(FETCH => -> $ { $!col }, STORE => -> $, $v { $!col = $v }) }
}

my $m = Model.new;
is $m.col<yes>:exists, True, 'a present key through an rw accessor';
is $m.col<no>:exists, False, 'an absent key';
is $m.col<no>:!exists, True, ':!exists';
is @asked.elems, 3, 'the answer\'s own Bool ran once per lookup';
for $m { is .col<yes>:exists, True, 'through the topic' }
my $blk = { .col<yes>:exists };
is $blk($m), True, 'inside a block';
