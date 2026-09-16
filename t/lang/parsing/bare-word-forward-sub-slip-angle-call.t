use Test;

plan 2;

# `sub-name |<a b>, ...` (no parens) is a listop call with a flattened
# `<...>` word-list argument. When the callee is declared LATER in the same
# scope (a forward reference -- legal in Raku, subs are visible throughout
# their enclosing block), the parser had not yet registered its name, and
# `|<...>` glued to `|` was not recognized as an unambiguous slip-prefix
# argument start (only `|$x`/`|@x`/`|%x`/`|&x`/`|(...)` were) -- so `test`
# was read as a bare 0-arg term and `|` as the infix any-junction operator
# instead. Found via Acme::Anguish's dependency Test::Output, whose
# `output-is`/`stdout-is`/etc. all call `test |<all is>, &?ROUTINE.name, |@args`
# with `test` declared further down the same module.
sub call-it (*@args) { forward-target |<all is>, |@args }

sub forward-target (Str:D $a, Str:D $b, Str:D $c, Str:D $d) {
    "$a-$b-$c-$d";
}

is call-it("x", "y"), 'all-is-x-y',
    'forward-referenced sub call takes a glued |<...> as a flattened argument';

# The infix any-junction reading must still work when there is no listop head
# to prefer a call: `bareword-value | <word-list>` (spaced) stays a junction.
my constant K = 5;
ok (K | <5 6>) == 5, 'K | <5 6> still reads as the any-junction over a term';
