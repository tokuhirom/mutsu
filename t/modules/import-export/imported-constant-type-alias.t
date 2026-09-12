use v6;
use lib 't/lib';
use Test;
use ConstantTypeAliasExport;

# A `constant` bound to a bare type name is a TYPE ALIAS -- it binds a type
# object into the lexical scope, and Raku accepts it anywhere a type name goes.
# mutsu imported the alias fine as a *value* (`AliasedInt.^name` was already
# `Int`) but lost its type-ness on the way, so every "is this name a type"
# validator rejected it: a sub parameter and a role-method parameter reported
# `Invalid typename`, and a variable declaration reported the alias as a
# package "insufficiently type-like to qualify a variable" (#8131).
#
# The class-method position was the odd one out and already worked, which is
# why it is pinned here too: its validator runs late enough to see the import.
#
# `Gnome::N`'s `constant \GType is export = uint64` is the real site, so the
# fixture carries all three shapes an alias comes in: to a builtin, to a native
# type (sigil-lessly spelled), and to a class the same module declares.

plan 12;

# --- the alias still imports as a value, as it always did ---
is AliasedInt.^name, 'Int', 'an imported alias to a builtin names its target';
is AliasedNative.^name, 'uint64', 'a sigilless alias to a native type names its target';
is AliasedShape.^name, 'ConstantTypeAliasExport::Shape', 'an alias to a class names its target';

# --- sub parameter (compile-time pre-pass) ---
sub takes-int(AliasedInt $x) { $x + 1 }
is takes-int(3), 4, 'an imported alias types a sub parameter';

sub takes-native(AliasedNative $x) { $x + 1 }
is takes-native(7), 8, 'a native-typed alias types a sub parameter';

sub takes-shape(AliasedShape $s) { $s.sides }
is takes-shape(Shape.new), 3, 'a class alias types a sub parameter';

# The pre-pass descends into nested blocks, so the alias has to survive there.
{
    sub nested(AliasedInt $x) { $x * 2 }
    is nested(4), 8, 'an imported alias types a sub parameter inside a nested block';
}

# --- return type ---
sub returns-int(--> AliasedInt) { 5 }
is returns-int(), 5, 'an imported alias types a sub return';

# --- role method, and the class method that already worked ---
{
    role R { method m(AliasedInt $x) { $x + 10 } }
    class C does R { }
    is C.new.m(1), 11, 'an imported alias types a role method parameter';

    class D { method m(AliasedInt $x) { $x + 20 } }
    is D.new.m(1), 21, 'an imported alias types a class method parameter';
}

# --- variable declaration: accepted, and still type-checked against the TARGET ---
{
    my AliasedInt $v = 3;
    is $v.WHAT.^name, 'Int', 'an imported alias types a variable declaration';
}

# rakudo reports the resolved target in the failure ("expected Int"), not the
# alias, which is what resolving the alias before the check gives.
throws-like { my AliasedInt $bad = "x" }, X::TypeCheck::Assignment,
    'and the declared variable is still type-checked against the target';
