use Test;

# From the Red distribution (`use Red` exports `infix:<>>` candidates with
# `is rw` parameters): a subscripted operand is passed as its container, and
# when no user candidate matches, the core operator must read its value.

plan 8;

role Node { }
multi infix:<< > >>(Numeric() $a is rw, Node $b) is export { False }
multi infix:<< < >>(Node $a, Numeric() $b is rw) is export { False }

my %h = value => 4;
my @a = 4, 5;
my $v = 4;

ok %h<value> > 3, "hash element > literal falls back to the core operator";
ok @a[0] > 3, "array element > literal";
ok $v > 3, "variable > literal";
nok %h<value> > 10, "hash element > larger literal";
ok %h<value> == 4, "hash element == literal";
ok @a[1] > @a[0], "two subscripted operands";
is-deeply @a.grep(* > 4), (5,).Seq, "WhateverCode over elements";
is-deeply (%(value => 5), %(value => 1)).grep(*<value> > 3).map(*<value>).Seq, (5,).Seq,
    "grep(*<value> > 3) over hashes";
