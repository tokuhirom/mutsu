use Test;

# From CSS::Module's CSS::Module::CSS3::Fonts: `<declarations=.Pkg::declarations>`
# calls a rule through a qualified name. The action that runs is the rule's own
# (`declarations`), not one named after the package path.

plan 3;

grammar Other { token declarations { \w+ } }

grammar G {
    token TOP { '@' <fd> }
    rule fd { <declarations=.Other::declarations> }
    token declarations { \d+ }
}

class A {
    method fd($/) { make $<declarations>.ast }
    method declarations($/) { make 'D:' ~ $/ }
}

my $m = G.subparse('@abc', :actions(A.new));
is ~$m, '@abc', 'the qualified call matches';
is $m<fd>.ast, 'D:abc', 'the callee rule action ran under its own name';
is $m<fd><declarations>.made, 'D:abc', 'the aliased capture carries the made value';
