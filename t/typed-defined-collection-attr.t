use Test;

# A `:D` type smiley on an array (`@`) or hash (`%`) attribute constrains the
# ELEMENTS, not the container. The container defaults to an empty (defined)
# Array/Hash and needs no initializer. mutsu used to demand an initializer
# ("Variable '@!rules' of type 'Callable:D' must be initialized"), which broke
# e.g. Path::Finder's `has Callable:D @!rules`.

plan 5;

class A {
    has Callable:D @!rules;
    method n { @!rules.elems }
}
is A.new.n, 0, ':D array attribute defaults to an empty array (no initializer needed)';

class B {
    has Int:D %!seen;
    method n { %!seen.elems }
}
is B.new.n, 0, ':D hash attribute defaults to an empty hash';

class C {
    has Str:D @.tags;
    method add($x) { @!tags.push: $x }
    method all { @.tags.join(',') }
}
my $c = C.new;
$c.add('x');
$c.add('y');
is $c.all, 'x,y', 'public :D array attribute is usable and pushable';

# Elements still honour the :D constraint when assigned.
class D {
    has Int:D @.nums;
}
lives-ok { D.new(nums => [1, 2, 3]) }, ':D array accepts defined elements';

# A scalar :D attribute with no default/`is required` must STILL error.
dies-ok { EVAL 'class S1 { has Int:D $!x }; S1.new' },
    'scalar :D attribute without initializer still errors';
