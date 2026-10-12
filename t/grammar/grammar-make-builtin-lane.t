use Test;

# An unshadowed one-argument `make` goes straight to the builtin
# (`try_make_builtin_call`) instead of through the generic call funnel. A
# user `sub make` or a lexical `&make` still wins, the `$/` precondition still
# throws, and a call with no positional still raises the funnel's binding error.

plan 7;

grammar G {
    token TOP { <a> <b> }
    token a { a }
    token b { b }
}

class Plain {
    method a($/) { my $made = 5; make 42; is $made, 5, 'make does not write a $made lexical' }
    method b($/) { make $/.Str.uc }
    method TOP($/) { make [$<a>.made, $<b>.made] }
}
is-deeply G.parse('ab', :actions(Plain)).made, [42, 'B'], 'make sets .made in action methods';

my @seen;
class Shadowed {
    sub make($x) { @seen.push: "sub:$x" }
    method a($/) { make 1 }
    method b($/) { my &make = -> $x { @seen.push: "lexical:$x" }; make 2 }
}
my $m = G.parse('ab', :actions(Shadowed));
is-deeply @seen, ['sub:1', 'lexical:2'], 'a user sub make and a lexical &make shadow the builtin';
is $m<a>.made, Nil, 'a shadowed make does not set .made';

sub outside { make 1 }
throws-like { outside() }, X::Make::MatchRequired, 'make without a Match in $/ throws';

'x' ~~ /x/;
make 7;
is $/.made, 7, 'make outside a grammar sets the current match .made';

throws-like { 'x' ~~ /x/; make :a(1) }, Exception,
    message => /'Too few positionals'/, 'make with only a named argument is a binding error';
