use Test;

# A zen slice is one operation whatever its brackets: `$x<>` is `$x[]`. It
# strips one level of itemization and leaves every other value as it is
# (compiled to the `DeitemizeZen` opcode, not a synthetic method call).

plan 16;

is (1..3)<>.raku, '1..3', 'a Range is itself under <>';
is (1..3)[].raku, '1..3', '... and under []';

my $a = [1, 2];
is $a<>.raku, '[1, 2]', 'an itemized Array under <>';
is $a[].raku, '[1, 2]', '... and under []';

my %h = a => 1;
is $%h<>.raku, '{:a(1)}', 'an itemized Hash under <>';
is $%h[].raku, '{:a(1)}', '... and under []';

my $x = slip(5, 6);
is $x<>.raku, 'slip(5, 6)', 'an itemized Slip under <>';
is $x[].raku, 'slip(5, 6)', '... and under []';

my $s = (1, 2).Seq;
is $s<>.raku, '(1, 2).Seq', 'a $-held Seq under <>';
is $s[].raku, '(1, 2).Seq', '... and under []';

my $z = (1, 2, 3).map(* + 1);
$z<>.iterator;
throws-like { $z.iterator }, X::Seq::Consumed, 'a zen slice does not cache a Seq';

my $y = 5;
is $y<>.raku, '5', 'a scalar is its value';
is Int<>.raku, 'Int', 'a type object is itself';

"abc" ~~ /b/;
is $/<>.Str, 'b', 'a Match is itself';
isa-ok $<>, Match, '... also as $<>';

my %g = a => 1, b => 2;
is-deeply %g<>:k.sort.List, <a b>, 'an adverb on a zen slice';
