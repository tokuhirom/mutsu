unit module DynamicDoesCompoundRole;

# Getopt::Long's exact shape (issue #8578): a lexically-scoped, dot-qualified
# role declared in this module, mixed into a value at runtime (dynamic `does`,
# not a class's static `does Parent`) from a sibling sub in the same file. The
# role's own compound short name (`Formatted::Named`, not fully qualified
# `DynamicDoesCompoundRole::Formatted::Named`) is what the dynamic `does`
# expression names.

my role Formatted::Named is export {
    has $.x;
}

sub apply($obj) is export {
    return $obj does Formatted::Named(:x(1));
}

# `Formatted::Named` is `my`-scoped: an importer cannot name it directly
# (raku itself reports "Could not find symbol" for `$obj ~~
# Foo::Formatted::Named` from outside this module), so the role-identity
# check has to live in here too.
sub applied-formatted-named($obj) is export {
    return $obj ~~ Formatted::Named;
}
