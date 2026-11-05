use Test;

# The `ParameterTarget::Term` of a sigilless capture parameter (`|c`) has the
# range of the name after the `|`, as in rakudo 2026.09 (#15780).

plan 7;

use experimental :rakuast;

sub rng($n) { my $o = $n.origin; $o ?? "{$o.from}-{$o.to}" !! "none" }
sub target(Str $src) {
    my $p = $src.AST.statements[0].expression.signature.parameters[0];
    "{rng($p)} {rng($p.target)} {rng($p.target.name)}"
}

is target("sub f(|c) \{ 1 }"), "6-8 7-8 7-8", 'a sub';
is target("method m(|c) \{ 1 }"), "9-11 10-11 10-11", 'a method';
is target("sub f(|args) \{ 1 }"), "6-11 7-11 7-11", 'a longer name';
is target("sub f(|c where \{ 1 }) \{ 1 }").words[1], "7-8", 'with a where clause';
is target("sub f(\\x) \{ 1 }"), "6-8 7-8 7-8", 'the sigilless term (unchanged)';
is target("sub f( |c ) \{ 1 }").words[1], "8-9", 'spaces around';
is target("multi sub f(|c) \{ 1 }").words[1], "13-14", 'a multi';
