use Test;

plan 7;

no worries;

my $manna = :a1:b2:c3;
is $manna.^name, 'Pair', 'an adjacent colonpair run stays a Pair';
is $manna.raku, ':a1', 'the first value-shorthand pair wins as the term value';

my $list = (:a1:b2:c3);
is $list.^name, 'List', 'an adjacent colonpair run in parens is a List';
is $list.elems, 3, 'the parenthesised list keeps all three pairs';

sub s(*%h) {
    is %h.elems, 2, 'a slurpy named argument receives both pairs';
    is %h<a1>, True, 'the first slurpy named pair is present';
    is %h<b2>, True, 'the second slurpy named pair is present';
}
s :a1:b2;
