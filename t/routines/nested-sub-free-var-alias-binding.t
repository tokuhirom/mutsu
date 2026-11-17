use v6;
use Test;

# A `my sub` declared in a routine reads its free variables through the
# declaring activation's alias, whoever calls it and whatever the caller has
# under the same name (#9111), on every read of a loop -- the lookup reads the
# binding in the same env walk that finds the alias (#17971).

plan 5;

sub tok(Str:D $html) {
    my int $len = $html.chars;
    my sub scan(Int:D $from) {
        my int $probe = $from;
        $probe++ while $probe < $len && $html.substr($probe, 1) ne ' ';
        $probe
    }
    my sub caller-with-same-names() {
        my $len = 1;
        my $html = 'x';
        scan(0)
    }
    (scan(0), caller-with-same-names())
}
is tok('abc def').join(','), '3,3', 'free variables read the declaring activation';

sub counter() {
    my $n = 0;
    my sub bump() { $n++ }
    my sub peek() { $n }
    bump() for ^4;
    peek()
}
is counter(), 4, 'a sibling sub sees a free variable another one writes';

sub outer(Int $k) {
    my $v = $k;
    my sub get() { $v }
    my sub rec(Int $d) { $d == 0 ?? get() !! outer($k + 1) ~ ':' ~ get() }
    $k < 2 ?? rec(1) !! get()
}
is outer(0), '2:1:0', 'each activation keeps its own free variable under recursion';

sub loop-shadow() {
    my $x = 'outer';
    my sub read-x() { $x }
    my @r;
    for <a b> -> $x { @r.push: read-x() }
    @r.join(',')
}
is loop-shadow(), 'outer,outer', 'a caller loop parameter of the same name does not leak in';

sub reread() {
    my $s = 'a';
    my sub get() { $s }
    my @r = get();
    $s = 'b';
    @r.push: get();
    @r.join(',')
}
is reread(), 'a,b', 'a later write by the declaring routine is seen';
