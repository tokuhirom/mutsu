use Test;

plan 4;

# A Junction held in a variable (a `Mu` parameter, a `my Mu` variable) must
# autothread over a multi whose candidates take Any / Str, like a literal
# junction argument does (#13148).
my multi g(Any $n) { "a$n" }
my multi g(Str $n) { "s$n" }

is g(any("a", "b")).gist, 'any(sa, sb)', 'a literal junction threads';
sub h(Mu $x) { g($x) }
is h(any("a", "b")).gist, 'any(sa, sb)', 'a Mu parameter holding a junction threads';
my Mu $m = any("a", "b");
is g($m).gist, 'any(sa, sb)', 'a `my Mu` variable threads';
my $j = any("a", "b");
is g($j).gist, 'any(sa, sb)', 'an untyped variable threads';
