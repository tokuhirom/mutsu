use Test;

plan 4;

grammar Repeated {
    token a { :my $fresh; <ident> }
}

is Repeated.subparse("foo", :rule<a>).pos, 3,
    "a token with an uninitialized :my scalar matches initially";
is Repeated.subparse("foo", :rule<a>).pos, 3,
    "the token still matches after its :my scalar exists in the caller environment";

my $y = "outer";
grammar Shadowed {
    token a { :my $y; <ident> }
}

is Shadowed.subparse("foo", :rule<a>).pos, 3,
    "a same-named outer scalar does not replace a token's :my declaration";
is Shadowed.subparse("foo", :rule<a>).pos, 3,
    "the shadowed token remains reusable";
