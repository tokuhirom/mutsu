use v6;
use Test;

# #9073: a routine that declares an inner `my sub` moves the functions-map
# generation on entry and back on exit. The call caches that used to be
# emptied on every such move -- the positional and named light-call caches,
# the OTF call cache, and the multi-sub resolution cache -- now keep one
# answer per generation instead, and the re-install hands back a memoized
# copy of the whole table. These are the shapes where a cache answering under
# the wrong generation, or a memoized table carrying the wrong content, would
# be a silent mis-dispatch.

plan 9;

# 1. Positional light calls on both sides of the excursion: `helper` means
#    the outer sub outside the routine and the inner one inside it, and both
#    call sites run many times so both answers are cached.
sub helper($x) { "outer:$x" }
sub shadowing($x) {
    my sub helper($y) { "inner:$y" }
    helper($x) ~ '|' ~ helper($x + 1);
}
my @got;
for ^5 -> $i {
    @got.push: helper($i);
    @got.push: shadowing($i);
}
is @got.join(','),
    (^5).map({ "outer:$_,inner:$_|inner:{$_ + 1}" }).join(','),
    'positional calls resolve per side of the excursion, every iteration';

# 2. Named-parameter calls take the named light-call cache. The routine's
#    inner sub is a different name, so the same outer sub is the answer on
#    both sides and must be found on both.
sub named-helper(:$v) { "named:$v" }
sub named-caller(:$v) {
    my sub decorate($s) { "[$s]" }
    decorate(named-helper(:$v));
}
my @named;
for ^4 -> $v {
    @named.push: named-helper(:$v);
    @named.push: named-caller(:$v);
}
is @named.join(','),
    (^4).map({ "named:$_,[named:$_]" }).join(','),
    'named calls resolve on both sides of the excursion';

# 3. A multi resolved from both sides: inside the routine the inner sub is a
#    second map state, so a winner cached for one state must not be served for
#    the other, and both must survive the alternation.
multi sub kind(Int $) { 'Int' }
multi sub kind(Str $) { 'Str' }
sub with-inner($x) {
    my sub unrelated() { 'u' }
    kind($x) ~ unrelated();
}
my @kinds;
for 1, 'a', 2, 'b' -> $x {
    @kinds.push: kind($x);
    @kinds.push: with-inner($x);
}
is @kinds.join(','), 'Int,Intu,Str,Stru,Int,Intu,Str,Stru',
    'multi winners are right on both sides, for both argument types';

# 4. Two routines installing same-named inner subs with different bodies:
#    their installs start from the same map and must land on different ones,
#    so a memoized table for one must never be handed to the other.
sub via-a() { my sub pick-one() { 'a' }; pick-one() }
sub via-b() { my sub pick-one() { 'b' }; pick-one() }
my $ab = (^6).map({ via-a() ~ via-b() }).join;
is $ab, 'ab' x 6, 'same-named inner subs of different routines stay distinct';

# 5. The inner sub is gone after the routine returns, however many times the
#    memoized table has been reused.
sub declares-gone() { my sub only-inside() { 'here' }; only-inside() }
declares-gone() for ^5;
is declares-gone(), 'here', 'the inner sub is callable inside';
throws-like { EVAL 'only-inside()' }, X::Undeclared::Symbols,
    'and not outside, after many reuses';

# 6. A routine that declares TWO inner subs installs twice per call; the
#    second install starts from the first one's memoized table.
sub two-inner($x) {
    my sub first-one($y)  { $y * 2 }
    my sub second-one($y) { $y + 1 }
    second-one(first-one($x));
}
is (^5).map({ two-inner($_) }).join(','), '1,3,5,7,9',
    'two inner subs per call';

# 7. Recursion re-enters the routine while its inner sub is installed.
sub recurse($n) {
    my sub step($m) { $m - 1 }
    $n <= 0 ?? 'done' !! recurse(step($n));
}
is recurse(5), 'done', 'recursion through a routine with an inner sub';
is recurse(3), 'done', 'and again';

done-testing;
