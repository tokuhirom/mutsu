use Test;
use lib 't/lib';
use DynamicDoesCompoundRole;

plan 3;

# Getopt::Long's exact shape (issue #8578): a lexically-scoped, dot-qualified
# role (`my role Formatted::Named`) declared inside a module, mixed into a
# value at RUNTIME (dynamic `does`, not a class's static `does Parent`) from a
# sibling sub in the same file, via the role's own compound short name
# (`Formatted::Named`), not its fully qualified `DynamicDoesCompoundRole::
# Formatted::Named`.
#
# mutsu parsed the dotted name in the dynamic `does` expression as a
# package-qualified call (`&Named` inside package `Formatted`) instead of
# resolving it to the single lexically-scoped role declared earlier in the
# same file, dying with "Could not find symbol '&Named' in
# 'GLOBAL::Formatted'". The class-declaration path
# (`t/oo/role/my-role-compound-name-qualification.t`) already handled this
# compound-name aliasing; the dynamic `does` call path did not.
#
# The same call also pins the role initializer's named argument: the role has
# one public attribute, so `:x(1)` must store `1`, not the `:x(1)` Pair itself.

my $r = apply(42);
is $r.^name, 'Int+{DynamicDoesCompoundRole::Formatted::Named}',
    'the dynamic does mixes in the compound-named role';
ok applied-formatted-named($r), 'the mixed-in value does the lexically-scoped role';
is $r.x, 1, 'a named role initializer stores the argument value';
