use Test;
plan 12;

# A `.map` block whose last statement is a subscript returns the element's
# container, and the Seq holds it: `.VAR` of the Seq element is a Scalar.
my %h = a => 1;
my @a = 5;
my $r = ('a',).map(-> $k { %h{$k} });
is $r[0].VAR.^name, "Scalar", "Seq subscript of a hash-element map";
is $r.first.VAR.^name, "Scalar", "first";
is ('a',).map(-> $k { %h{$k} }).first(*.defined).VAR.^name, "Scalar", "first with a matcher";
is ('a',).map({ %h<a> })[0].VAR.^name, "Scalar", "a bare block";
is (1,).map({ @a[0] })[0].VAR.^name, "Scalar", "an array element";
my $w = (1,).map({ @a[0] }); $w[0] = 9;
is-deeply @a, [9], "the cell writes through";

# Values that are not containers stay bare.
is ('a',).map(-> $k { 7 })[0].VAR.^name, "Int", "a literal result";
is (1,2,3).map(* + 1)[1].VAR.^name, "Int", "an expression result";
is (1,2,3).grep(* > 1)[0].VAR.^name, "Int", "grep over a literal list";
is (1,2,3).grep(* > 1).list[0].VAR.^name, "Int", "grep over a literal list, .list";
my @l = 1, 2;
is @l.grep(* > 0)[0].VAR.^name, "Scalar", "grep over an Array keeps the element cells";
is (1,2,3).map(* + 1)[5], Nil, "past the end";
