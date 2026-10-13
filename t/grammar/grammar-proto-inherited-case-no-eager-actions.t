use Test;

# From CSS::Module: a rule with `:i` that calls a proto rule must not run the
# proto candidate's `||` alternatives that the match never reaches. The
# candidate used to be evaluated eagerly (every end computed up front), which
# fired the action of the unreached `<any-args>` branch.

plan 3;

my @called;

grammar G {
    token any-args { <[\d,]>+ }
    proto rule color {*}
    rule color:sym<rgb> { 'rgb(' [ \d+ || <any-args> ] ')' }
    rule inside-i  { :i [ <color> || 'x' ] }
    rule outside-i { [ <color> || 'x' ] }
}

class A {
    method any-args($/) { @called.push: ~$/ }
}

my $m = G.subparse('rgb(10)', :rule<inside-i>, :actions(A.new));
is ~$m, 'rgb(10)', ':i caller matches through the proto';
is @called.elems, 0, 'the unreached || branch fires no action under :i';

@called = ();
G.subparse('rgb(10)', :rule<outside-i>, :actions(A.new));
is @called.elems, 0, 'same without :i';
