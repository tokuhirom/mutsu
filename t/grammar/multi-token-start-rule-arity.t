use v6;
use Test;

# From CSS::Specification (t/defs.t): a grammar with `multi token keyw {..}`
# beside `multi token keyw($rx) {..}` started via `.subparse(:rule<keyw>)`
# with no arguments must pick the candidate the (empty) arguments fit.

plan 5;

grammar G {
    token Ident { <[a..z]>+ }
    multi token keyw        { <id=.Ident> }
    multi token keyw($rx)   { <id={$rx}> }
}

is G.subparse("abc", :rule<keyw>).Str, "abc", 'zero-arg multi candidate selected as start rule';
is G.subparse("abc", :rule<keyw>, :args(("abc",))).Str, "abc", ':args selects the one-arg candidate';

# A Str-based enum key subscripts a Pair by its string form.
my Str enum T (:I<int>, :J<num>);
my $p = (I) => 123;
is $p{I}, 123, 'Pair{enum key} matches by the enum string value';
ok $p{I}:exists, 'Pair{enum key}:exists';
nok $p{J}.defined, 'Pair{other enum key} is Nil';
