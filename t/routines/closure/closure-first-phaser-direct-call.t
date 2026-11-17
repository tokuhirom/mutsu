use Test;

# A block's FIRST belongs to the code object: the first call of each closure
# clone runs it at entry, before ENTER, whoever makes the call -- a direct
# `$c()`, a routine call, or a `.map`/`.grep` loop (#17945). Checked against
# rakudo.

plan 9;

my $g = 0;
my $c = { FIRST $g++; 1 };
$c() for ^3;
is $g, 1, 'a block called directly fires its FIRST once';

my @log;
sub f { FIRST @log.push('f'); 1 }
f() for ^2;
is-deeply @log, ['f'], 'a sub fires its FIRST on its first call only';

@log = ();
for ^2 {
    my $d = { FIRST @log.push('fresh'); 1 };
    $d(); $d();
}
is-deeply @log, ['fresh', 'fresh'], 'each fresh closure clone fires its own FIRST';

@log = ();
my $e = { @log.push('body'); FIRST @log.push('first'); ENTER @log.push('enter') };
$e();
is-deeply @log, ['first', 'enter', 'body'], 'FIRST runs at entry, before ENTER';

@log = ();
my @r = (1..3).map({ FIRST @log.push("map $_"); $_ });
is-deeply @log, ['map 1'], 'a .map block sees its first element as the topic in FIRST';
is-deeply @r, [1, 2, 3], '... and maps every element';

@log = ();
my $shared = { FIRST @log.push('shared'); $_ };
(1..2).map($shared).eager;
$shared(5);
(1..2).grep($shared).eager;
is-deeply @log, ['shared'], 'one clone fires once across .map, a direct call and .grep';

@log = ();
for ^2 { do { FIRST @log.push('do'); ENTER 1 } }
is-deeply @log, ['do', 'do'], 'an inlined block fires FIRST on every entry';

@log = ();
my $p = -> $x { FIRST @log.push("pointy $x"); $x * 2 };
my @doubled = (4, 5).map($p);
is-deeply (@log, @doubled), (['pointy 4'], [8, 10]), 'a pointy block binds its parameter before FIRST';
