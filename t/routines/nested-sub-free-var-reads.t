use Test;

# A sub declared inside a routine reads the declaring call's lexicals, and
# keeps reading them live: mutsu caches which binding a free variable
# resolved to for the duration of one call of the nested sub (mutsu#17971).
# These pin that the cache never answers with a stale or foreign binding.

plan 12;

sub scanner(Str $s) {
    my int $len = $s.chars;
    my $hits = 0;
    my sub count-to(Int $from) {
        my int $i = $from;
        $i++ while $i < $len;
        $i
    }
    my sub bump() { $hits }
    (count-to(0), count-to(2), $len)
}
is-deeply scanner('abcde'), (5, 5, 5), 'a nested sub reads its outer variable in a loop';
is-deeply scanner('xy'), (2, 2, 2), 'a second call of the outer routine reads its own';

# The outer variable changes between and during nested calls.
sub changing() {
    my $n = 1;
    my sub get() { $n }
    my sub get-twice-around(&mutate) { my $a = $n; mutate(); my $b = $n; "$a/$b" }
    my @seen;
    @seen.push: get();
    $n = 2;
    @seen.push: get();
    @seen.push: get-twice-around({ $n = 3 });
    @seen.join(',')
}
is changing(), '1,2,2/3', 'reads see writes made between and during calls';

# Recursion: each activation of the declaring routine has its own variable.
sub depth(Int $d) {
    my $mine = "d$d";
    my sub show() { $mine }
    my $inner = $d > 0 ?? depth($d - 1) !! '';
    show() ~ ($inner ?? ",$inner" !! '')
}
is depth(3), 'd3,d2,d1,d0', 'each recursive activation reads its own binding';

sub depth-after(Int $d) {
    my $mine = "a$d";
    my sub show(&between) { my $first = $mine; between(); "$first:$mine" }
    show({ depth-after($d - 1) if $d > 0 })
}
is depth-after(2), 'a2:a2', 'a recursive call in the middle of a read does not swap the binding';

# The caller's same-named variables never answer.
sub owner() {
    my $v = 'owner';
    my sub read-v() { $v }
    my $from-block = do { my $v = 'block'; read-v() };
    my @from-loop = (for <loop1 loop2> -> $v { read-v() });
    ($from-block, |@from-loop).join(',')
}
is owner(), 'owner,owner,owner', 'a shadowing block or loop variable of the caller is not read';

# A code reference to the nested sub, called after the outer returned.
sub make-reader() {
    my $x = 'captured';
    my sub reader() { $x }
    &reader
}
my &r = make-reader();
is r(), 'captured', 'a nested sub called through its code object';
is r() ~ r(), 'capturedcaptured', 'repeatedly';

# A nested sub that writes its free variable, and one that only reads it.
sub writer() {
    my $c = 0;
    my sub inc() { $c++ }
    my sub peek() { $c }
    inc() for ^3;
    my $p1 = peek();
    inc();
    "$p1/{peek()}"
}
is writer(), '3/4', 'a reader sees a sibling writer';

# An integer that becomes Nil and back.
sub nil-ish() {
    my $v = 5;
    my sub get() { $v // 'undef' }
    my @r = get();
    $v = Nil;
    @r.push: get();
    $v = 7;
    @r.push: get();
    @r.join(',')
}
is nil-ish(), '5,undef,7', 'a variable set to Nil and back';

# Several free variables in one nested sub, in a tight loop.
sub sum-range(Int $lo, Int $hi) {
    my $step = 1;
    my $total = 0;
    my sub run() { my $i = $lo; my $t = 0; while $i <= $hi { $t += $i; $i += $step }; $t }
    run()
}
is sum-range(1, 100), 5050, 'several free variables read in a loop';

# A dynamic variable is never served from the cache.
sub dyn() {
    my $*D = 'outer';
    my sub read-d() { $*D }
    my @r = read-d();
    { my $*D = 'inner'; @r.push: read-d() }
    @r.join(',')
}
is dyn(), 'outer,inner', 'a dynamic variable keeps its dynamic lookup';
