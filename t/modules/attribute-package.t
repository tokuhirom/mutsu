use Test;

plan 9;

# A `has` attribute declared in a module/package body is illegal: a package
# cannot hold attributes. This is X::Attribute::Package, distinct from the
# mainline X::Attribute::NoPackage.

throws-like 'my module A { has $.x }', X::Attribute::Package,
    package-kind => 'module';

throws-like 'package Y { has $.foo }', X::Attribute::Package,
    package-kind => 'package';

# Public ($.x) and private ($!x) twigils both error.
throws-like 'module M { has $!secret }', X::Attribute::Package,
    package-kind => 'module';

# The reported attribute name keeps its sigil + twigil.
throws-like 'package P { has $.foo }', X::Attribute::Package,
    name => '$.foo';

# unit form extends the package to the rest of the scope.
throws-like 'unit module U; has $.x;', X::Attribute::Package,
    package-kind => 'module';

# The mainline `has` (no enclosing package) stays NoPackage.
throws-like 'has $.x', X::Attribute::NoPackage;

# A class/role/grammar legitimately holds attributes.
lives-ok { EVAL 'class C { has $.x = 5 }' }, 'class can have attributes';
lives-ok { EVAL 'role R { has $.y = 6 }' }, 'role can have attributes';
lives-ok { EVAL 'grammar G { has $.z = 7; token TOP { . } }' },
    'grammar can have attributes';
