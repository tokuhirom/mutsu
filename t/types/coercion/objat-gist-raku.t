use Test;

# An ObjAt / ValueObjAt (the result of `.WHICH`) gists (and stringifies) as the
# bare WHICH string `Int|42`; only `.raku` / `.perl` show the constructor form
# `ValueObjAt.new("Int|42")`. Previously mutsu had these swapped: `.gist` showed
# the constructor form and the nested `.raku` showed the bare WHICH.

plan 10;

is 42.WHICH.Str, 'Int|42', '.WHICH.Str is the bare WHICH';
is 42.WHICH.gist, 'Int|42', '.WHICH.gist is the bare WHICH';
is 42.WHICH.raku, 'ValueObjAt.new("Int|42")', '.WHICH.raku is the constructor form';

# say uses .gist.
my $w = 42.WHICH;
is "$w", 'Int|42', '.WHICH interpolates as the bare WHICH';

is "abc".WHICH.gist, 'Str|abc', 'Str .WHICH.gist';
is "abc".WHICH.raku, 'ValueObjAt.new("Str|abc")', 'Str .WHICH.raku';

# Nested in containers: gist shows WHICH, raku shows constructor form.
is [42.WHICH].gist, '[Int|42]', 'nested .gist shows the WHICH';
is [42.WHICH].raku, '[ValueObjAt.new("Int|42")]', 'nested .raku shows the constructor';
is { a => 1.WHICH }.raku, '{:a(ValueObjAt.new("Int|1"))}', 'hash-value .raku';

# .WHICH equality still works.
ok "x".WHICH eq "x".WHICH, '.WHICH values compare equal';
