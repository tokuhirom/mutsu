use Test;

# A `"..."` atom in a regex reads `$_` as the topic the regex runs under: the
# subject of `~~`, or the caller's `$_` for `.match` / `.subst`, a code block in
# it too (`/ "{ $_ }" /`; #18738, roast S05-mass/rx.t test 654).

plan 7;

$_ = 'foo bar';
is ~m/ "{ $_ }" /, 'foo bar', 'm// with a block reading the topic';

my $t = 'zzab';
$_ = 'ab';
is ~($t ~~ / "$_" /), 'zzab', '~~ topicalizes the subject for "$_"';
is ~($t ~~ / "{ $_ }" /), 'zzab', 'and for "{ $_ }"';
is ~('zzab'.match(/ "{ $_ }" /)), 'ab', '.match reads the caller\'s $_';
is 'zzab'.subst(/ "{ $_ }" /, 'X'), 'zzX', '.subst too';
given 'zz' {
    nok 'azz' ~~ /a "{ $_ }"/, 'a given topic is replaced by the ~~ subject';
}
is ~('a3' ~~ / "a{ 1 + 2 }" /), 'a3', 'a block that ignores the topic';
