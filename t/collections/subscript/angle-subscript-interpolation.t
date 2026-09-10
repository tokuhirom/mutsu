use v6;
use Test;

# A postcircumfix guillemet / double-angle subscript (`%h«...»`, `%h<<...>>`) is an
# interpolating quote-word list (qqww semantics). Quoted words and sigil variables
# inside the subscript must interpolate exactly as in a standalone `«...»` term.
# Regression: `%h«oranges "$fruit"»` used to keep `"$fruit"` literal (doc-diff
# hashmap.rakudoc [2]).

plan 10;

my %h = oranges => 'round', bananas => 'bendy', '1' => 'one';
my $fruit = 'bananas';

# Bare variable interpolation.
is-deeply %h«oranges $fruit».List, ('round', 'bendy'), 'bare $var in « » subscript';
is-deeply %h<<oranges $fruit>>.List, ('round', 'bendy'), 'bare $var in << >> subscript';

# Double-quoted interpolating word.
is-deeply %h«oranges "$fruit"».List, ('round', 'bendy'), 'quoted "$var" in « » subscript';
is-deeply %h<<oranges "$fruit">>.List, ('round', 'bendy'), 'quoted "$var" in << >> subscript';

# Single-word subscript stays scalar (not a one-element list).
is %h«oranges», 'round', 'single word « » subscript is scalar';
is %h<<oranges>>, 'round', 'single word << >> subscript is scalar';
is %h«$fruit», 'bendy', 'single interpolated word « » subscript is scalar';

# Numeric-looking keys work (allomorphic qqww, stringified for hash lookup).
is %h«1», 'one', 'numeric word « » subscript';

# Plain non-interpolating literal words still work.
is-deeply %h«oranges bananas».List, ('round', 'bendy'), 'plain multi-word « » slice';

# Non-interpolating <...> keeps literal $-text (no interpolation).
my %g = '$fruit' => 'literal';
is %g<$fruit>, 'literal', 'plain < > subscript does not interpolate';
