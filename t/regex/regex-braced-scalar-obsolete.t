use Test;

# `${name}` in a regex is Perl 5 syntax: rakudo rejects it at compile time
# with X::Obsolete, suggesting `$x` (or `{$x}` inside a `"..."` literal)
# (#18925). `${ code }` is the end anchor followed by a code block.

plan 7;

throws-like q[my $x = "a"; "xa" ~~ / x ${x} /], X::Obsolete,
    message => 'Unsupported use of ${x}. In Raku please use: $x.',
    'a bare ${name} is obsolete';

throws-like q[my $x = "a"; "xa" ~~ / x "${x}" /], X::Obsolete,
    message => 'Unsupported use of ${x}. In Raku please use: {$x}.',
    'inside a double-quoted literal the replacement is {$x}';

throws-like q[my $x = "a"; "xa" ~~ m/ x ${x} /], X::Obsolete,
    'the m// form is rejected too';

ok "x\$\{x}" ~~ / x '${x}' /, 'a single-quoted ${x} is literal text';

nok "xa" ~~ / x ${ True } /, '${ code } is the end anchor and a block';

my $x = "a";
ok "xa" ~~ / x $x /, 'a plain $x still interpolates';

my $src = 'x ${x}';
throws-like { "xa" ~~ / <$src> / }, X::Obsolete,
    'a ${name} in run-time regex source is rejected the same way';
