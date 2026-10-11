use v6;
use Test;

# #12788: Metamodel::PrivateMethodContainer's find_private_method and
# private_method_names, pinned against raku.

plan 8;

class A {
    method !p($x) { 42 + $x }
    method !q { 1 }
}
class B is A {
    method !r { 2 }
}

my $m = A.^find_private_method("p");
is $m(A, 1), 43, 'the found private method is callable with an invocant';
is $m.name, 'p', 'it is the method named p';
is $m.^name, 'Method', 'it is a Method';
nok A.^find_private_method("nope").defined, 'an unknown name finds nothing';
nok B.^find_private_method("p").defined, 'an inherited private method is not searched';
ok B.^find_private_method("r").defined, 'the class\'s own private method is found';
is-deeply A.^private_method_names.sort.list, ("p", "q"), 'private_method_names lists the own private methods';
is-deeply B.^private_method_names.list, ("r",), 'private_method_names omits inherited ones';
