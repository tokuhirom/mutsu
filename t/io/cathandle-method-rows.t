use Test;

# `IO::CatHandle`'s methods are rows of `IO::CatHandle`, found by their owner;
# a read through any receiver advances the one cat (ADR-11276 §9.132).

plan 7;

my $dir = $*TMPDIR.add("cathandle-rows-$*PID");
mkdir $dir;
my $a = $dir.add('a.txt');
my $b = $dir.add('b.txt');
$a.spurt("one\ntwo\n");
$b.spurt("three\n");

my $cat = IO::CatHandle.new($a, $b);
is $cat.get, 'one', 'get through a variable';
my @cats = $cat;
is @cats[0].get, 'two', 'get through an array element advances the same cat';
is $cat.get, 'three', 'the next handle';
ok $cat.eof, 'eof';

my $lines = IO::CatHandle.new($a, $b);
is-deeply $lines.lines.List, ('one', 'two', 'three'), 'lazy lines';
is IO::CatHandle.new($a).slurp, "one\ntwo\n", 'slurp of a temporary';
ok IO::CatHandle.new($a).can('next-handle'), '.can finds a row';

$a.unlink;
$b.unlink;
$dir.rmdir;
