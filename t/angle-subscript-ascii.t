use v6;
use Test;

# ASCII `<<...>>` postcircumfix subscript is the interpolating twin of `«...»`
# (Rakudo word-quote subscript semantics). Regression pin for dist Resource::Wrangler.

my $key = "a";
my %h = a => 1, b => 2, literal => 3;

# single interpolated key
is %h<<$key>>, 1, '%h<<$key>> interpolates the variable';
is %h«$key», 1, '%h«$key» interpolates the variable';

# with surrounding whitespace inside the brackets
is %h<< $key >>, 1, '%h<< $key >> tolerates inner whitespace';

# multi-word slice with interpolation
is-deeply %h<<$key literal>>.List, (1, 3), '%h<<$key literal>> slices with interpolation';
is-deeply %h<<$key b>>.List, (1, 2), '%h<<$key b>> slices two interpolated/literal keys';
is-deeply %h«$key literal».List, (1, 3), '%h«$key literal» slices with interpolation';

# literal (non-interpolating) single-angle stays literal
is %h<literal>, 3, '%h<literal> uses the literal key';

# infix hyper operators (needing surrounding whitespace) are unaffected
my @a = 1, 2, 3;
my @b = 10, 20, 30;
is-deeply (@a <<+>> @b).List, (11, 22, 33), 'infix <<+>> hyper op still works';

# chained subscripts
my %nested = a => { x => 42 };
is %nested<<$key>><x>, 42, 'chained <<$key>><x> subscript';

done-testing;
