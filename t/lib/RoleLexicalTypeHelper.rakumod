unit role RoleLexicalTypeHelper;

# A lexical TYPE declaration (`my grammar`, `my class`) in a role body is a
# lexical declaration of the role's own compilation unit, exactly like a
# private `sub` -- it must be usable from an exported sub of the same role
# without the role ever being composed onto a class (`does`/`is`). See
# t/modules/import-export/role-body-lexical-type-used-from-export.t.

my grammar Greeting {
    token TOP { .* }
}

my class GreetingActions {
    method TOP($/) { make "hello, " ~ $/.Str; }
}

sub greet-private(Str $name) {
    return Greeting.parse($name, actions => GreetingActions.new).made;
}

sub greet(Str $name) is export {
    return greet-private($name);
}
