use Test;

# `Metamodel::PrivateMethodContainer.private_methods`: a class's own private
# methods (and private submethods) as Method objects, in declaration order,
# named without the `!` sigil. Reduced from the Manifest::StopWar
# distribution, which walks `self.^private_methods` and calls each entry back
# with `self!"$name"()`.

plan 12;

role R { method !r { 'r' } }
class P { method !inh { 1 } }
class A is P {
    method !zeta { "z" }
    method !alpha(--> Str) { "a" }
    method !mid { "m" }
    submethod !sub { "s" }
    method pub { }
    method call-all { self.^private_methods.map({ my $n = .gist; self!"$n"() }).List }
}
class E { method pub { } }
class RC does R { }

is-deeply A.^private_methods.map(*.name).List, <zeta alpha mid sub>,
    'own private methods and submethods, in declaration order';
is-deeply A.^private_methods.map(*.gist).List, <zeta alpha mid sub>,
    '.gist of each entry is its bare name';
is A.^private_methods.elems, 4, 'public methods and inherited privates are excluded';
isa-ok A.^private_methods[0], Method, 'entries are Method objects';
isa-ok A.^private_methods[3], Submethod, 'a private submethod is a Submethod';
is A.new.^private_methods.elems, 4, 'works on an instance too';
is-deeply A.new.call-all, <z a m s>, 'each name calls back through self!"$name"()';
is-deeply P.^private_methods.map(*.name).List, ('inh',), 'parent sees only its own';
is E.^private_methods.elems, 0, 'class without private methods gives an empty list';
is-deeply RC.^private_methods.map(*.name).List, ('r',), 'composed role private methods are included';

# The `!` is call syntax, not part of the name: the same holds for
# `.^private_method_table` values and `.^find_private_method`.
is A.^private_method_table<zeta>.name, 'zeta', 'private_method_table entry .name has no !';
is A.^find_method('pub').name, 'pub', 'public method name unchanged';
