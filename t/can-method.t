use Test;

plan 15;

# .can on built-in types
ok "foo".can("split"), '.can on built-in Str finds split';
ok !"foo".can("hazcheezburger"), '.can returns false for non-existent method';

# .can on user-defined classes
class Dog {
    method bark { "bow" }
}
my $dog = Dog.new;

ok $dog.can("bark"), '.can finds user-defined method';
ok !$dog.can("w00f"), '.can returns false for non-existent method on class';
is $dog.can("bark").elems, 1, '.can returns 1 method';

# Returned methods are callable
my $meth = $dog.can("bark");
is $meth[0]($dog), "bow", 'method from .can is callable';

# Subclass MRO walking
class Puppy is Dog {
    method bark { "yap" }
}
my $pup = Puppy.new;
is $pup.can("bark").elems, 2, '.can on subclass returns 2 methods (own + parent)';

my $found = "";
for $pup.can("bark") -> $m {
    $found ~= $m($pup);
}
is $found, "yapbow", 'methods returned in MRO order';

# ^can (HOW dispatch)
ok "bar".^can("split"), '^can works on built-in types';
ok Dog.can("bark"), '.can on type object works';
ok !Dog.can("w00f"), '.can returns false for missing method on type object';

# .invert on Bag and Set
my $b = bag("a", "a", "b");
is $b.invert.elems, 2, 'Bag.invert returns correct number of pairs';

my $s = set("a", "b");
is $s.invert.elems, 2, 'Set.invert returns correct number of pairs';

# .resume on exceptions
my $resumed = 0;
try {
    die "ohh";
    $resumed = 1;
    CATCH { default { .resume } }
}
is $resumed, 1, '.resume continues after die';

# .does on values
ok 42.does(Int), '.does returns True for matching type';
