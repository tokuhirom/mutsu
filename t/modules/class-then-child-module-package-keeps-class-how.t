use Test;
use lib 't/fixtures/class-then-child-package/lib';
use PkcParent;

# `unit class Parent; use Parent::Child;` where the child module is
# `unit package Parent;` must leave Parent a class (Async::Workers, which
# AttrX::Mooish refused as "not a Raku role or class").

plan 5;

is PkcParent.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'the parent keeps ClassHOW';
is PkcParent.new.value, 42, 'and instantiates as a class';
is PkcParent.new.how-name, 'Perl6::Metamodel::ClassHOW', 'also from inside its own method';
ok PkcParent::Child.new.defined, 'the child module class is still reachable';
ok PkcParent ~~ Mu && PkcParent.^can('new'), 'the parent has a constructor';
