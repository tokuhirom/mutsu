use v6;
use Test;

# Inside a double-quoted regex literal, a `$var.method(...)` chain is a
# qq-string method-call interpolation (Raku interpolates `"$x.uc()"`), not a
# scalar followed by a match-any `.`. Regression: mutsu interpolated only the
# bare `$var` and left `.method()` as literal regex, so nothing matched.
# (raku-doc Language/regexes.rakudoc, "Regexes / Regex interpolation")

plan 6;

my $p = 'gnirts';

is ('a string here' ~~ / "$p.flip()" /).Str, 'string',
    '$p.flip() interpolates as a method call inside "..."';

is ('a STRING x' ~~ / "$p.flip().uc()" /).Str, 'STRING',
    'chained .flip().uc() interpolates';

# A method call with arguments (nested parens handled).
my $word = 'hello';
is ('xhelx' ~~ / "$word.substr(0,3)" /).Str, 'hel',
    'method call with arguments interpolates';

# A bare `.method` WITHOUT parens is NOT an interpolation: the literal text
# (var value + ".method") is matched.
is ('gnirts.foo' ~~ / "$p.foo" /).Str, 'gnirts.foo',
    'a paren-less .foo stays literal inside "..."';

# Plain interpolation with no method still works.
is ('zgnirtsz' ~~ / "$p" /).Str, 'gnirts',
    'plain $p interpolation inside "..." unaffected';

# The documented example.
my $string = 'Is this a regex or a string: 123';
is ($string.match(/ "$p.flip()" /)).Str, 'string',
    'documented $pattern.flip() example';
