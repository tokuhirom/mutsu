use v6;
use Test;

# A bare block never declares a return type, so it must not inherit — and
# enforce — the `--> T` of the routine or pointy block it is written inside.
#
# Regression: the closure-construction path for a bare block `{ ... }` kept a
# lexically captured `__mutsu_return_type`, so a block argument written inside a
# `--> Pair` routine failed the *outer* Pair check on its own inner value:
#
#   -> $x --> Pair { (@k.map({ $x{$_} }).join: ":") => $x }
#      # Type check failed for return value; expected Pair but got Str
#
# (found via Text::CSV's `csv(:key[...])`, which builds exactly this shape)

plan 6;

my %row = bar => "1", baz => "2";
my @k = <bar baz>;

my $pointy = -> $x --> Pair { (@k.map({ $x{$_} }).join: ":") => $x };
is $pointy(%row).^name, 'Pair',
    'a nested block inside a `--> Pair` pointy block does not inherit the check';
is $pointy(%row).key, '1:2', 'and the pair is built from the nested block results';

sub named($x --> Pair) { (@k.map({ $x{$_} }).join: ":") => $x }
is named(%row).^name, 'Pair',
    'a nested block inside a `--> Pair` sub does not inherit the check';

my $two = -> $x, $y --> Pair { (@k.map({ $x{$_} }).join: $y) => $x };
is $two(%row, '-').key, '1-2',
    'the same holds for a multi-parameter pointy block';

# The enclosing routine's own return type is still enforced.
dies-ok { my $f = -> $x --> Pair { @k.map({ $x{$_} }).join(":") }; $f(%row) },
    'the outer return type is still checked on the outer result';

# A nested block that declares its own return type still enforces it.
dies-ok { my $f = -> $x --> Pair { my $g = -> --> Int { "s" }; $g(); 'k' => $x }; $f(%row) },
    'a nested block with its own return type still enforces that one';
