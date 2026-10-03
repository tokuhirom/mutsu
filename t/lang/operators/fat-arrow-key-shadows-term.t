use Test;

# `ident => value` is a Pair whose key is the literal identifier, even when
# that identifier names a declared term (Raku's `<?before \h* '=>'>`
# fat-arrow lookahead). mutsu parsed a user `sub term:<data-home>` in
# `C.new(data-home => ...)` as a call of the term, so the constructor got a
# positional Pair and died "only takes named arguments". Reduced from
# XDG::BaseDirectory's t/006-terms-dynamic.t, which builds the object with
# the same names its `:terms` import exports as terms.

plan 9;

sub term:<data-home> { 'TERM' }
my \sigilless = 5;

class C { has $.data-home; has $.sigilless }
my $c = C.new(data-home => 'x', sigilless => 'y');
is $c.data-home, 'x', 'named argument whose name is a user term';
is $c.sigilless, 'y', 'named argument whose name is a sigilless variable';

is-deeply (data-home => 1), (:data-home(1)), 'term name before => is a pair key';
is-deeply (data-home=>1), (:data-home(1)), 'no whitespace before =>';
is-deeply (sigilless => 2), (:sigilless(2)), 'sigilless name before => is a pair key';
is-deeply (pi => 3), (:pi(3)), 'built-in term before => is a pair key';

is data-home, 'TERM', 'the term is still a term elsewhere';
is sigilless, 5, 'the sigilless variable is still readable';

# Only horizontal whitespace: a newline before `=>` leaves the term a term.
my $p = (data-home
    => 4);
is $p.key, 'TERM', 'a newline before => keeps the term';
