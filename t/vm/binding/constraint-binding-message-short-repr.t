use Test;

plan 5;

# A "Constraint type check failed in binding" message renders the offending
# value like the plain "Type check failed" one: a long string is cut to 19
# characters plus `...`, and the explanation is word-wrapped.
my $s = "12345678901234567890111";
subset Name of Str where *.chars < 20;

sub by-subset(Name $x) { }
try by-subset($s);
is $!.message,
    q{Constraint type check failed in binding to parameter '$x'; expected Name but got Str ("1234567890123456789...)},
    'a subset refinee failure cuts a long string';

sub by-where(Str $x where *.chars < 3) { }
try by-where($s);
is $!.message,
    "Constraint type check failed in binding to parameter '\$x'; expected anonymous constraint to be met but got Str\n"
        ~ q{("1234567890123456789...)},
    'a where failure cuts a long string and wraps the explanation';

subset Big of Int where * > 5;
sub by-big(Big $x) { }
try by-big(3);
is $!.message,
    q{Constraint type check failed in binding to parameter '$x'; expected Big but got Int (3)},
    'a short value is shown whole';

my $twenty = "x" x 20;
try by-subset($twenty);
like $!.message, / '("xxxxxxxxxxxxxxxxxxxx")' /, 'twenty characters are still shown whole';

my $int = 42;
try by-subset($int);
like $!.message, / ^ 'Type check failed in binding' .* 'Int (42)' /, 'a value of another type is the plain failure';

done-testing;
