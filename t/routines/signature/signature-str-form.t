use Test;

plan 8;

# Rakudo's Signature declares `gist` and `raku`, not `Str`, so `.Str` falls to
# Mu.Str: the `Signature<address>` form (#13109).
my $sig = :(Int $x);

like $sig.Str, /^ 'Signature<' \d+ '>' $/, '.Str is the Mu.Str form';
like $sig.Stringy, /^ 'Signature<' \d+ '>' $/, '.Stringy is the Mu.Str form';
like ~$sig, /^ 'Signature<' \d+ '>' $/, 'prefix ~ is the Mu.Str form';
like "$sig", /^ 'Signature<' \d+ '>' $/, 'interpolation is the Mu.Str form';
is $sig.gist, '(Int $x)', '.gist stays the source form';
is $sig.raku, ':(Int $x)', '.raku stays the source form';
is $sig.Str, $sig.Str, '.Str is stable for one object';
isnt :(Int $y).Str, $sig.Str, 'distinct signatures differ';
