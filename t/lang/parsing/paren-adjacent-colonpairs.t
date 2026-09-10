use Test;

# Space-separated colonpairs inside parentheses form a list without commas.
# The `:$var` sigil-shorthand form must be recognized as an adjacent colonpair
# just like the explicit `:name(...)` form (WebService::Nominatim regression).

plan 9;

my $a = 1;
my $b = 2;
my $c = 3;

# Pure adjacency with sigil shorthand.
is (:$a :$b).raku, '(:a(1), :b(2))', 'adjacent :$a :$b';

# Comma then adjacency.
is (:$a, :$b :$c).raku, '(:a(1), :b(2), :c(3))', 'comma then adjacent';

# Adjacency then comma.
is (:$a :$b, :$c).raku, '(:a(1), :b(2), :c(3))', 'adjacent then comma';

# Mixed sigil-shorthand and explicit-paren forms.
is (:a(1) :$b).raku, '(:a(1), :b(2))', 'paren-form then sigil-shorthand';
is (:$a :b(2)).raku, '(:a(1), :b(2))', 'sigil-shorthand then paren-form';

# Array / hash / callable sigils.
my @arr = 1, 2;
my %hsh;
is (:@arr :%hsh).raku, '(:arr([1, 2]), :hsh({}))', 'array and hash sigils';

# Assigned into a hash — the WebService::Nominatim %query pattern.
my %q = (
    :$a,
    :$b
    :$c,
);
is %q.keys.sort.join(','), 'a,b,c', 'colonpairs into hash with mixed commas';

# Explicit-paren colonpairs still work (no regression).
is (:x(1) :y(2)).raku, '(:x(1), :y(2))', 'explicit paren colonpairs';

# Plain comma list is unaffected.
is (1, 2, 3).raku, '(1, 2, 3)', 'plain comma list unaffected';
