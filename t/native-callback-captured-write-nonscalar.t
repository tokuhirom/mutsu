use Test;

# `call_sub_value(..., merge_all: True)` is the call path native Rust code
# uses to invoke a Raku callable (reduce's block, a Promise/Supply callback,
# `IO::CatHandle`'s `on-switch`, an lvalue Proxy's FETCH/STORE, ...). Nothing
# else drains the resulting caller-slot writeback right after such a call, so
# the merge itself must track *every* value type the callback writes to a
# captured outer variable -- not just scalars -- or the caller's cached local
# slot goes stale until some unrelated later call happens to flush it.
plan 4;

sub reduce-capture-list() {
    my $captured;
    [1, 2, 3].reduce: { $captured = ($^a, $^b); $^a + $^b };
    $captured;
}
is-deeply reduce-capture-list(), (3, 3), 'reduce callback write of a List to a captured outer var lands immediately';

sub reduce-capture-nil() {
    my $captured = 'unset';
    [1, 2].reduce: { $captured = Nil; $^a + $^b };
    $captured;
}
is reduce-capture-nil(), Nil, 'reduce callback write of Nil to a captured outer var lands immediately';

{
    my @seen;
    my &cb = -> \a, \b { @seen.push: (a, b) };
    &cb(1, 2);
    &cb(Nil, Nil);
    is-deeply @seen, [(1, 2), (Nil, Nil)], 'plain closure call still tracks list writes (no regression)';
}

{
    my $captured;
    my &cb = -> \a, \b { $captured = (a, b) };
    sub call-it(&f) { f(Nil, Nil) }
    call-it(&cb);
    is-deeply $captured, (Nil, Nil), 'closure invoked one level removed still writes back a List of Nils';
}
