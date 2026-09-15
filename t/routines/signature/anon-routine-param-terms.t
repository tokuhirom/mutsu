use Test;

# An anonymous routine literal's own parameters have to be in scope while its
# BODY is parsed. A sigilless parameter declares a TERM and a `&name` parameter
# declares a routine, so inside the body a bare `m` IS that binding. A named
# `sub`/`method` declaration already registered both; the anonymous literal did
# not, so its body was parsed against what the name means OUTSIDE it.
#
# For a name that also spells a quote-like operator that is not merely a wrong
# lookup but a different LEX: `[m, z, k]` read `m` as a match, took `,` for its
# delimiter and swallowed the SIGNATURE's own `\z` as a regex escape, so the
# reported error was "Unsupported use of \z as end-of-string matcher" pointing
# at a construct several lines up. That is what `PDF::Content`'s
# `method ( \c, \m, \y, \k) { @!FillColor = [ c, m, y, k ] }` hit (#7954).

plan 12;

class Holder { has @.slots is rw }

# The reduced form: three sigilless parameters whose names spell quote-like
# operators, all read in one list in the body.
my $cmyk = sub (\c, \m, \y, \k) { [ c, m, y, k ] };
is-deeply $cmyk(1, 2, 3, 4), [1, 2, 3, 4], 'sub literal: sigilless m and y are terms in the body';

my $method = method (\c, \m, \y, \k) { [ c, m, y, k ] };
is-deeply $method(Holder.new, 5, 6, 7, 8), [5, 6, 7, 8],
    'method literal: sigilless m and y are terms in the body';

# One at a time, so a regression names the culprit.
is (sub (\m) { [m, 0] })(9).raku, '[9, 0]', 'a lone sigilless m';
is (sub (\y, \k) { [y, k] })(1, 2).raku, '[1, 2]', 'sigilless y and k';
is (sub (\s, \q, \tr) { [s, q, tr] })(1, 2, 3).raku, '[1, 2, 3]', 'sigilless s, q and tr';
is (sub (\Q, \rx, \m) { [Q, rx, m] })(1, 2, 3).raku, '[1, 2, 3]', 'sigilless Q, rx and m';

# A sigilless parameter also shadows a builtin listop, which is the same
# registration by a different consequence.
is (sub (\join, \index) { [join, index] })(1, 2).raku, '[1, 2]',
    'a sigilless parameter shadows a builtin listop';

# A `&name` parameter makes a bare `name()` a call to the bound routine rather
# than an `m//` match.
is (sub (&m) { m() })(sub { 'called' }), 'called', 'a &m parameter is a routine in the body';
is (method (&s, \x) { s(x) })(Holder.new, { $_ * 2 }, 21), 42,
    'method literal: a code parameter and a sigilless one together';

# A destructuring sub-signature's sigilless parameters count too.
is (sub ((\m, \y)) { [m, y] })((1, 2)).raku, '[1, 2]',
    'sigilless parameters inside a sub-signature';

# The names stay local to the literal: outside it, `m` is the match operator
# again.
my $outer = 'abcd';
ok $outer ~~ m/bc/, 'a bare m// still parses outside the literal';

# And the method literal really does receive its invocant, past the registered
# terms.
class Store {
    has @.slots is rw;
    method installer { method (\c, \m) { @!slots = [c, m]; @!slots } }
}
my $store = Store.new;
is-deeply $store.installer.($store, 'x', 'y'), ['x', 'y'],
    'the method literal writes through its invocant';
