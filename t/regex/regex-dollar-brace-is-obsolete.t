use Test;

# #18925: `${name}` in a regex is Perl 5's braced scalar. rakudo refuses it at
# compile time with X::Obsolete (suggesting `$name`, or `{$name}` inside a
# `"..."` literal); mutsu used to read it as `$name` and splice its value.

plan 8;

throws-like q[my $x = "a"; so "xa" ~~ / x ${x} /], X::Obsolete,
    old => '${x}', replacement => '$x',
    'a braced scalar in a regex is X::Obsolete';
throws-like q[my $x = "a"; so "xa" ~~ / x "${x}" /], X::Obsolete,
    old => '${x}', replacement => '{$x}',
    'inside a "..." literal the suggestion is a code block';
throws-like q[say 1; my $x = "a"; so "xa" ~~ / ${x} /], X::Obsolete,
    'it is raised before the program runs';

my $x = "a";
ok "xa" ~~ / x $x /, 'a plain $x still interpolates';
ok "a\n" ~~ / a $$ /, '$$ is still the end-of-line anchor';
ok "x" ~~ / . $${ 1 } /, '$$ before a code block is not a braced scalar';
ok 'x${y}' ~~ / x '${y}' /, 'a single-quoted ${y} is literal text';

my $built = 'x ${x}';
throws-like { "xa" ~~ / <$built> / }, X::Obsolete,
    'a pattern built at run time is refused the same way';
