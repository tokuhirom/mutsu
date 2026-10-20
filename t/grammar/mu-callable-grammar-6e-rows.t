use v6.e.PREVIEW;
use Test;

# Language revision 6.e adds `Mu.Callable($method)` and Grammar's
# `locprepost`, `typed_exception` and `SETFAIL`.
plan 12;

my $abs = 42.Callable("abs");
isa-ok $abs, Method, 'Callable answers a method object';
is $abs(-3), 3, 'called with an invocant it runs the method';
class A { method twice($x) { $x * 2 } }
is A.Callable("twice").name, 'twice', 'a user method, from a type object';
is A.new.Callable("twice")(A.new, 4), 8, 'a user method, from an instance';
throws-like { 42.Callable("nope") }, X::Method::NotFound, 'a missing method';

grammar G { token TOP { a } }
is G.^can("typed_exception").elems, 1, 'typed_exception exists';
is G.^can("SETFAIL").elems, 1, 'SETFAIL exists';
is G.^can("locprepost").elems, 1, 'locprepost exists';

my $g = G.new(:orig("abc\ndef"));
is-deeply $g.locprepost, ["<BOL>", "abc"], 'locprepost at the start of a line';
is-deeply G.new(:orig("")).locprepost, ["<BOL>", "<EOL>"], 'locprepost of an empty text';
is G.SETFAIL(Nil), Nil, 'SETFAIL on a type object is Nil';

is-deeply G.new(:orig("x" x 100)).locprepost, ["<BOL>", "x" x 40], 'the text after is cut at 40 characters';
