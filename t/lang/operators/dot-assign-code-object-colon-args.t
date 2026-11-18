use Test;

# `$x .= &f: args` is the colon-argument form of `$x = $x.&f(args)`
# (CSS::Specification::Compiler's Grammars.rakumod writes
# `$regex-body .= &quantified: $_ with $quant;`).

plan 6;

sub suffix($x, *@rest) { $x ~ @rest.join }

my $a = 'a';
$a .= &suffix: '+';
is $a, 'a+', '.= &f: one argument';

my $b = 'b';
$b .= &suffix: '*', '?';
is $b, 'b*?', '.= &f: several arguments';

my $c = 'c';
my $q = '!';
$c .= &suffix: $_
    with $q;
is $c, 'c!', '.= &f: argument list ends before a statement modifier';

my $d = 'd';
$d .= &suffix: 'x' if False;
is $d, 'd', 'the modifier guards the whole statement';

my @l = 1, 2;
sub add(@a, $n) { (|@a, $n) }
@l .= &add: 3;
is-deeply @l, [1, 2, 3], 'an array target';

my $e = 'e';
$e .= &suffix('(', ')');
is $e, 'e()', 'the parenthesized form still works';
