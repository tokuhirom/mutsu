use v6;
use Test;

# A typed aggregate parameter requires a typed container, not just a container
# whose current elements happen to satisfy the element constraint.

plan 16;

sub pos(Int @a) { @a.elems }
is pos(my Int @ = 1, 2, 3), 3, 'typed positional literal is accepted';
is pos(Array[Int].new(1, 2, 3)), 3, 'parameterized Array is accepted';
throws-like { pos([1, 2, 3]) }, X::TypeCheck::Binding::Parameter,
    'plain Array is rejected even when all elements are Int';
my @plain = 1, 2, 3;
throws-like { pos(@plain) }, X::TypeCheck::Binding::Parameter,
    'untyped Array variable is rejected';
throws-like { my Int @untyped := Array.new(1, 2, 3) }, X::TypeCheck::Binding,
    ':= binding from an untyped Array is rejected';
my Int @bound := Array[Int].new(1, 2, 3);
is @bound.elems, 3, ':= binding from a parameterized Array is accepted';

sub assoc(Int %h) { %h.elems }
is assoc(my Int % = a => 1, b => 2), 2, 'typed associative literal is accepted';
is assoc(Hash[Int].new((a => 1, b => 2))), 2, 'parameterized Hash is accepted';
throws-like { assoc({a => 1, b => 2}) }, X::TypeCheck::Binding::Parameter,
    'plain Hash is rejected even when all values are Int';
my %plain = a => 1, b => 2;
throws-like { assoc(%plain) }, X::TypeCheck::Binding::Parameter,
    'untyped Hash variable is rejected';

class User7774 { has Int $.value }
sub user-pos(User7774 @a) { @a.elems }
throws-like { user-pos([User7774.new(:value(1))]) }, X::TypeCheck::Binding::Parameter,
    'plain Array of user objects is rejected';
my User7774 @typed = User7774.new(:value(1));
is user-pos(@typed), 1, 'typed Array of user objects is accepted';
is user-pos(Array[User7774].new(User7774.new(:value(2)))), 1,
    'parameterized Array of user objects is accepted';
throws-like { user-pos([User7774.new(:value(1)), 42]) }, X::TypeCheck::Binding::Parameter,
    'a wrong user-container element is rejected';

class UserRow7774 {
    has User7774 @.fields;
}
my $row = UserRow7774.new;
is user-pos($row.fields), 0,
    'a typed aggregate public accessor is accepted';
$row.fields = [User7774.new(:value(3))];
is user-pos($row.fields), 1,
    'a typed aggregate public accessor keeps its type after assignment';
