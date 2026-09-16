use Test;
use lib 't/lib';
use DynamicDoesCompoundRole;

plan 2;

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
# NB: this only pins the symbol-resolution bug (#8578). A related but
# separate bug (#8577) still loses the role's named-arg-parametrized
# attribute value on a dynamic `does`, so `.x` is not asserted here.

my $r = apply(42);
is $r.^name, 'Int+{DynamicDoesCompoundRole::Formatted::Named}',
    'the dynamic does mixes in the compound-named role';
ok applied-formatted-named($r), 'the mixed-in value does the lexically-scoped role';
