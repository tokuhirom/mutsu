use Test;

# Found in Math::NIntegrate, whose UnitCubeRescaling builds source text from
# `&f.signature.params.map(*.name)`: a positional placeholder parameter's
# `.name` is the plain `$x`, without the `^` twigil.

plan 6;

my &f = { $^x + $^y ** 2 };
is &f.signature.params.map(*.name).List, ('$x', '$y'), 'placeholder names carry no twigil';
is &f.signature.params.map(*.name.substr(1)).List, <x y>, 'and the bare names are x and y';
is { $^a }.signature.params[0].name, '$a', 'a single placeholder';
is { $:n }.signature.params[0].name, '$n', 'a named placeholder';
my &g = -> $p, $q { 1 };
is &g.signature.params.map(*.name).List, ('$p', '$q'), 'explicit parameters are unchanged';
is &f.signature.raku, ':($x, $y)', 'the signature still renders the same';
