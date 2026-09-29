use Test;

# From the Test::Builder distribution (t/01-load.rakutest): `isnt` compares
# stringifications, so two distinct instances with equal attributes must not
# stringify alike. Rakudo's Mu.Str on an instance is `Name<identity>`.

plan 6;

class A { has $.x; }
my $a = A.new;
my $b = A.new;

like $a.Str, /^ 'A<' \d+ '>' $/, 'Str of a plain instance is Name<identity>';
is $a.Str, $a.Str, 'same object stringifies the same';
isnt $a.Str, $b.Str, 'distinct instances stringify differently';
ok $a ne $b, 'distinct instances are not eq';
isnt $a, $b, 'isnt sees distinct instances';
is $a.gist, 'A.new(x => Any)', 'gist is unchanged';
