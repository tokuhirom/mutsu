use Test;

# GH-15259 (annotations 0.1.0): the anonymous one-arg slurpy `+` takes a
# sub-signature after a space, `+ [$a, $b]` / `+ ($a, $b)`.
plan 5;

sub bracket(+ [$a, $b]) { "$a $b" }
sub paren(+ ($a, $b)) { "$a $b" }
is bracket(1, 2), '1 2', '+ [..] unpacks the positional arguments';
is bracket([3, 4]), '3 4', 'or the single list argument';
is paren(5, 6), '5 6', '+ (..) likewise';

multi coerce(+ [Int(Str:D) $n, Str $s]) { "{$n.WHAT.^name} $s" }
is coerce("7", "x"), 'Int x', 'typed and coercing sub-parameters';
sub tight(+[$a, $b]) { "$a$b" }
is tight(8, 9), '89', 'the unspaced form still works';
