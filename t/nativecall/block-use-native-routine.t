use v6;
use lib 't/lib';
use Test;

# A native routine a module exports and a block `use`s still runs its native
# body: the lexical import set calls it through the routine's wrapper (#14140).
plan 3;

{
    use PgLikeNative;
    my $h = make-handle("hello");
    ok $h.defined, 'a native sub imported in a block returns a handle';
    is $h.strlen, 5, 'and its native method works';
    free-handle($h);
}

{
    my $count = 0;
    for 1..2 {
        use PgLikeNative;
        my $h = make-handle("ab");
        $count += $h.strlen;
        free-handle($h);
    }
    is $count, 4, 'a loop body that uses the module calls it every iteration';
}
