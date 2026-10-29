use v6;
use Test;

# BSON::Simple: a distribution's `role Hash::Ordered` is loaded by a program
# and, again, by a module that `use`s it. mutsu's built-in prelude class of
# that name must not be injected over the role the second time
# ("Redeclaration of symbol 'Hash::Ordered'").

plan 3;

use lib 't/lib/hash-ordered-role';
use Hash::Ordered;
use UsesHashOrdered;

is role-name(), 'Hash::Ordered', 'a module using the same-named role loads';
is Hash::Ordered.^name, 'Hash::Ordered', 'the program still sees it';
{
    use Hash::Ordered;
    pass 'a nested scope can use it again';
}
