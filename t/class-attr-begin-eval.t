use Test;

plan 8;

# A `has` declared at compile time via a BEGIN-phaser EVAL inside a class body
# attaches to the class (the class is still open at BEGIN time).
class C1 { BEGIN EVAL q[has $.x] };
is C1.new(x => 3).x, 3, 'BEGIN EVAL q[has $.x] declares a usable attribute';

# A BEGIN block (no EVAL) containing `has` also declares the attribute.
class C2 { BEGIN { has $.y } };
is C2.new(y => 7).y, 7, 'BEGIN { has $.y } declares a usable attribute';

# A CHECK-phaser EVAL is also compile time and declares the attribute.
class C3 { CHECK EVAL q[has $.z] };
is C3.new(z => 9).z, 9, 'CHECK EVAL q[has $.z] declares a usable attribute';

# A *plain* runtime EVAL inside a class body runs after the class is composed,
# so the attribute is NOT added and the accessor throws X::Method::NotFound.
class C4 { EVAL q[has $.w] };
throws-like { C4.new(w => 1).w }, X::Method::NotFound,
    'plain runtime EVAL q[has $.w] does NOT declare an attribute';

# Mainline `has $.x` (no package) throws X::Attribute::NoPackage.
throws-like 'has $.a', X::Attribute::NoPackage,
    'mainline has $.a throws X::Attribute::NoPackage';

# `has` inside a module body throws X::Attribute::Package.
throws-like 'module M { has $.a }', X::Attribute::Package,
    'has inside a module body throws X::Attribute::Package';

# A compile-time-declared attribute honors its type constraint via .new.
class C5 { BEGIN EVAL q[has Int $.n] };
is C5.new(n => 42).n, 42, 'typed attribute from BEGIN EVAL works';

# The compile-time attribute also gets a default when none is passed.
class C6 { BEGIN EVAL q[has $.d = 100] };
is C6.new.d, 100, 'default value on BEGIN EVAL attribute works';
