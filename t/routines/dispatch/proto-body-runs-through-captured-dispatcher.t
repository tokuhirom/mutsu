use Test;

# A proto with a body beyond `{*}` runs when its family is called through a
# code object taken where the family is declared, even after that scope is
# gone: the code object's `{*}` reaches the candidates it captured
# (ADR-8751 stage 2).
plan 4;

proto top-p(|c) { 'P:' ~ {*} }
multi top-p(Int $x) { "i$x" }
my &top = &top-p;
is top(1), 'P:i1', 'a top-level family through its code object';

my &blk = do {
    proto blk-p(|c) { 'Q:' ~ {*} }
    multi blk-p(Int $x) { "i$x" }
    multi blk-p(Str $x) { "s$x" }
    &blk-p
};
is blk(2), 'Q:i2', 'a block-local family after its block exited';
is blk('x'), 'Q:sx', 'with the other candidate';

my &wrap = do {
    proto blk-w($x) { my $r = {*}; "[$r]" }
    multi blk-w(Int $x) { $x * 2 }
    &blk-w
};
is wrap(5), '[10]', 'a proto that post-processes its {*} result';
