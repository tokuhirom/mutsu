use v6;
use Test;

plan 4;

# The type-constraint registry is keyed by bare name; an UNTYPED
# expression-position declaration (`ok ((my @r = ...), ...)`) must not
# inherit a constraint left by an unrelated same-named typed lexical —
# including one registered DURING the initializer (Text::CSV's
# `getline_all` runs `my Int @r = @!crange` internally, and the caller's
# `(my @r = $csv.getline_all($fh))` then rejected its own rows;
# t/79_callbacks.t aborted at test 31).

sub inner() { my Int @r = 1, 2; @r.elems }
inner();
sub pass-through($x, $) { $x }

my $got = pass-through((my @r = ["a"], ["b"]), "t");
is-deeply @r, [["a"], ["b"]], 'untyped expr-position decl ignores a stale same-named Int constraint';
ok $got, 'the declaration expression yields the value';

sub rows() { my Int @q = 3, 4; ([<x y>], [<z w>]) }
my $got2 = pass-through((my @q = rows()), "t");
is-deeply @q, [[<x y>], [<z w>]], 'a constraint registered DURING the initializer does not apply either';

# A genuinely typed expression-position declaration still enforces.
my $died = False;
try { pass-through((my Int @t = "nope",), "t"); CATCH { default { $died = True } } }
ok $died, 'a typed expr-position decl still type-checks its elements';
