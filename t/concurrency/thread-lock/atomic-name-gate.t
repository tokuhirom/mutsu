use Test;

# Pins the per-NAME gate in front of the atomic-variable read and reset paths
# (#12120, `runtime::atomic_names`). Registering one `atomicint` used to send
# every variable of the program through the name-keyed atomic cascade; now only
# the names registered as atomic take it. Rakudo has no such gate, so what this
# file pins is mutsu's own guarantee: every case where the name an atomic is
# registered under and the name it is read or reset through can differ gives the
# answer rakudo gives.

plan 10;

# A plain variable that shares its spelling with an atomic in another scope.
sub atomic-scope {
    my atomicint $n = 0;
    $n⚛++ for ^5;
    $n
}
my $n = 10;
$n += 1;
is atomic-scope(), 5, 'an atomic in a routine counts';
is $n, 11, 'a same-named plain variable outside it is untouched';

# A bound alias of an atomic reads what the atomic holds.
my atomicint $a = 0;
my $b := $a;
$a⚛++;
$a⚛++;
is $b, 2, 'a := alias of an atomic sees its updates';

# An atomic passed `is rw` is bumped under another name.
my atomicint $c = 0;
sub bump($v is rw) { $v⚛++ }
bump($c) for ^5;
is $c, 5, 'an atomic bumped through an `is rw` parameter';

# A closure bumping the atomic as a free variable, from several threads.
my atomicint $d = 0;
my &inc = { $d⚛++ };
await (^4).map({ start { inc() for ^250 } });
is $d, 1000, 'a captured atomic bumped from four threads';

# Unrelated locals read and written around an atomic in a hot loop.
my atomicint $f = 0;
my int $sum = 0;
for 1..1000 -> int $i {
    $f⚛++;
    $sum += $i;
}
is "$f $sum", "1000 500500", 'plain locals around an atomic in a loop';

# `cas` on one atomic while a plain variable is reassigned.
my atomicint $g = 5;
my $h = 1;
$h = 2;
my $seen = cas($g, 5, 9);
$h = 3;
is "$seen $g $h", "5 9 3", 'cas beside a reassigned plain variable';

# A redeclaration after the atomic exists is a fresh variable.
{
    my atomicint $r = 0;
    $r⚛++;
    is $r, 1, 'an atomic in a block';
}
{
    my atomicint $r = 0;
    my $plain = 3;
    $plain = $plain + 1;
    is "$r $plain", "0 4", 'a redeclared atomic starts fresh, a plain one beside it works';
}

# An atomic attribute bumped by a method and read through its accessor.
class Counter {
    has atomicint $.count = 0;
    method bump { $!count⚛++ }
}
my $counter = Counter.new;
$counter.bump for ^7;
is $counter.count, 7, 'an atomic attribute through method and accessor';
