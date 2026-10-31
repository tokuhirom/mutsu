use Test;

# PDF::IO::Serializer's t/io-serialize.t sorts with
#   sub obj-sort { my ($na, $ga) = @( $^a.value ); my ($nb, $gb) = @( $^b.value ); ... }
# Two bugs: (1) a placeholder used only inside a `my (...) = EXPR` initializer was not
# collected, so the sub took no arguments ("sort requires a callable with at least 1
# parameter"); (2) a Pair held in a variable, passed to a sub whose only parameters are
# positional placeholders, was treated as a named argument ("Too few positionals").

plan 11;

sub init-single { my ($x) = $^a; $x }
sub init-parens { my ($x) = ($^a); $x }
sub init-pair { my ($x, $y) = $^a, 1; $x }
sub init-method { my ($x) = @($^a.value); $x }
is (&init-single, &init-parens, &init-pair, &init-method).map(*.arity).join(' '),
    '1 1 1 1', 'a placeholder in a `my (...) =` initializer is a parameter';
is init-method((a => [7, 8])), 7, 'and it binds';

sub two-lists {
    my ($x, $y) = @( $^a.value );
    my ($p, $q) = @( $^b.value );
    $x <=> $p || $y <=> $q
}
is &two-lists.arity, 2, 'two destructured placeholders';
my @l = (a => [2, 1]), (b => [1, 5]), (c => [1, 2]);
is @l.sort(&two-lists).map(*.key).join(','), 'c,b,a', 'sorting with it';
is-deeply two-lists(@l[0], @l[1]), More, 'called directly with pairs from an array';

my $p = (a => 1);
sub key-of { $^x.key }
is key-of($p), 'a', 'a Pair in a variable is a positional argument';
is key-of((b => 2)), 'b', 'a parenthesized Pair too';
sub keys-of { $^x.key ~ $^y.key }
is keys-of(@l[0], @l[1]), 'ab', 'two of them';
dies-ok { key-of(a => 1) }, 'a colonpair-style literal is still a named argument';

# A `$:name` placeholder still receives its pair.
sub named-ph { $:size }
is named-ph(size => 3), 3, 'a named placeholder receives its colonpair';

sub mixed { $^x ~ ($:flag // '-') }
is mixed('v', flag => 'f'), 'vf', 'positional and named placeholders together';
