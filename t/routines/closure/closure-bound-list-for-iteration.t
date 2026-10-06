use v6;
use Test;

# `my $l := (1, 2, 3)` binds the List straight to the name: there is no Scalar
# container, so `for $l` iterates the elements. Whether `for $x` iterates once
# (a Scalar item) or element-wise is decided when the `for` is compiled, from the
# set of container-less `:=` bindings the compiler has seen. A routine or closure
# that reads such a variable as a free variable is compiled by a child compiler,
# which used to start with an empty set and so took the bound List for one item.

plan 17;

my $list := (1, 2, 3);
my $arr  := [4, 5];
my $seq  := (1, 2, 3, 4).map({ $_ });
my $item = (7, 8, 9);

sub over-list { my $n = 0; for $list { $n++ }; $n }
sub over-arr  { my $n = 0; for $arr  { $n++ }; $n }
sub over-seq  { my $n = 0; for $seq  { $n++ }; $n }
sub over-item { my $n = 0; for $item { $n++ }; $n }

is over-list(), 3, 'a sub iterates the List its captured scalar is bound to';
is over-arr(),  2, 'a sub iterates the Array its captured scalar is bound to';
is over-seq(),  4, 'a sub iterates the Seq its captured scalar is bound to';
is over-item(), 1, 'an ordinary `=` scalar holding a List stays one item in a sub';

my $m = 0; for $list { $m++ };
is $m, 3, 'the declaring scope iterates the bound List (unchanged)';

sub nested { my $n = 0; my &c = -> { for $list { $n++ } }; c(); $n }
is nested(), 3, 'a closure inside a sub sees the binding two levels down';

my &lam = -> { my $n = 0; for $list { $n++ }; $n };
is lam(), 3, 'an anonymous sub iterates the bound List';

sub block-in-sub { my $n = 0; { for $list { $n++ } }; $n }
is block-in-sub(), 3, 'a nested block inside a sub iterates the bound List';

# A declaration in the child shadows the inherited binding.
sub shadow-my { my $list = (1, 2, 3); my $n = 0; for $list { $n++ }; $n }
is shadow-my(), 1, 'a sub-local `my $list = ...` is a Scalar item again';

sub shadow-param($list) { my $n = 0; for $list { $n++ }; $n }
is shadow-param((1, 2, 3)), 1, 'a parameter of the same name is a Scalar item';

sub shadow-copy($list is copy) { my $n = 0; for $list { $n++ }; $n }
is shadow-copy((1, 2, 3)), 1, 'an `is copy` parameter of the same name is a Scalar item';

sub shadow-loop { my $n = 0; for ((1, 2, 3),) -> $list { for $list { $n++ } }; $n }
is shadow-loop(), 1, 'a loop parameter of the same name is a Scalar item';

# A bind to another captured container-less scalar stays container-less.
sub rebind { my $copy := $list; my $n = 0; for $copy { $n++ }; $n }
is rebind(), 3, '`my $copy := $captured` inside a sub stays container-less';

{
    my $l := (1, 2, 3);
    my $n = 0;
    my sub inner { for $l { $n++ } }
    inner();
    is $n, 3, 'a lexical sub in a bare block iterates the bound List';
}

module M {
    my $l := (1, 2, 3);
    our sub f { my $n = 0; for $l { $n++ }; $n }
    our sub g { my $n = 0; for ($l) { $n++ }; $n }
}
is M::f(), 3, 'a module routine iterates its block\'s `:=` binding';
is M::g(), 3, 'the parenthesized `for ($l)` spelling does too';

# The other direction: a name bound container-less in a SIBLING sub is not
# inherited by a sub that merely shares its name.
sub sibling-a { my $x := (1, 2, 3); my $n = 0; for $x { $n++ }; $n }
sub sibling-b { my $x = (1, 2, 3); my $n = 0; for $x { $n++ }; $n }
is sibling-a() ~ sibling-b(), '31', 'a sibling sub\'s own `:=` binding does not leak';
