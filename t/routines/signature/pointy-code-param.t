use Test;

plan 14;

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

# Inside the block the alias shadows an outer routine of that name and stops
# shadowing when the given block's lexical scope exits.
sub named() { 'outer' }
given { 'inner' } -> &named {
    is named(), 'inner', 'the block alias shadows an outer sub of that name';
}
is named(), 'outer', 'the code alias stops shadowing after the given body';

# Arguments pass through the alias.
given -> $x, $y { $x ~ $y } -> &joiner {
    is joiner('a', 'b'), 'ab', 'arguments reach the aliased code';
}

throws-like { given 'f0' -> &bad { flunk 'given body must not run' } },
    X::TypeCheck::Binding::Parameter, 'given rejects a non-Callable code parameter';
throws-like { with 'f0' -> &bad { flunk 'with body must not run' } },
    X::TypeCheck::Binding::Parameter, 'with rejects a non-Callable code parameter';
throws-like { for ('f0',) -> &bad { flunk 'for body must not run' } },
    X::TypeCheck::Binding::Parameter, 'for rejects a non-Callable code parameter';
throws-like { given 'f0' -> &bad is copy { flunk 'copy body must not run' } },
    X::TypeCheck::Binding::Parameter, 'copy code parameters also require Callable';
throws-like { for (1, 'f0') -> $n, &bad { flunk 'multi-param body must not run' } },
    X::TypeCheck::Binding::Parameter, 'multi-parameter for also checks code parameters';
