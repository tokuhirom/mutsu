use Test;

plan 8;

# A `&`-sigil pointy-block parameter declares a fresh lexical code alias.
# `given $code -> &f { … }` used to desugar to a bare `&f := $_`, which Raku
# rejects as "Code items cannot be rebound" — so the block died with
# X::Assignment::RO before running its first statement.

my $code = { 'called' };

given $code -> &f {
    is f(), 'called', 'given -> &f can be called by bare name';
    is &f(), 'called', 'given -> &f can be called through the & sigil';
    ok &f ~~ Callable, 'the alias is the Callable itself';
}

with $code -> &g {
    is g(), 'called', 'with -> &g binds the same way';
}

for ($code,) -> &h {
    is h(), 'called', 'for -> &h still works';
}

sub takes(&i) { i() }
is takes($code), 'called', 'a sub signature &param still works';

# Inside the block the alias shadows an outer routine of that name. (That it
# should also stop shadowing at the closing brace is a separate, pre-existing
# gap: mutsu's `given` block does not open a lexical scope for `my` at all, so
# `given 1 { my $z = 5 }` leaks `$z` too — see PLAN 8.22.)
sub named() { 'outer' }
given { 'inner' } -> &named {
    is named(), 'inner', 'the block alias shadows an outer sub of that name';
}

# Arguments pass through the alias.
given -> $x, $y { $x ~ $y } -> &joiner {
    is joiner('a', 'b'), 'ab', 'arguments reach the aliased code';
}
