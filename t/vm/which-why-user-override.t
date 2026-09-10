use v6;
use Test;

# A user-defined `method WHICH`/`method WHY` is an ordinary, overridable
# method in raku -- unlike the other six MOP pseudo-methods (DEFINITE, WHAT,
# WHO, HOW, WHERE, VAR), which stay compile-time macros regardless of call
# syntax. Only WHICH (custom value-identity semantics) and WHY (the Pod-doc
# accessor) must dispatch to the user override in EVERY call form: bareword
# (`.WHICH`), quoted-literal (`.'WHICH'()`), and dynamic (`."$m"()`).
# See todo/deep/pseudo-method-which-why-user-override-ignored-in-bareword-and-dynamic-form.md.

plan 14;

class Foo {
    method WHICH { "USER-WHICH" }
    method WHY { "USER-WHY" }
}

my $m1 = "WHICH";
my $m2 = "WHY";

# Instance receiver, all call forms.
is Foo.new.WHICH, "USER-WHICH", 'bareword .WHICH calls the user override on an instance';
is Foo.new.'WHICH'(), "USER-WHICH", "quoted .'WHICH'() calls the user override on an instance";
is Foo.new."$m1"(), "USER-WHICH", 'dynamic ."$m"() calls the user override on an instance';

is Foo.new.WHY, "USER-WHY", 'bareword .WHY calls the user override on an instance';
is Foo.new.'WHY'(), "USER-WHY", "quoted .'WHY'() calls the user override on an instance";
is Foo.new."$m2"(), "USER-WHY", 'dynamic ."$m"() calls the user override on an instance';

# Type-object receiver, all call forms.
is Foo.WHICH, "USER-WHICH", 'bareword .WHICH calls the user override on the type object';
is Foo.'WHICH'(), "USER-WHICH", "quoted .'WHICH'() calls the user override on the type object";
is Foo."$m1"(), "USER-WHICH", 'dynamic ."$m"() calls the user override on the type object';

is Foo.WHY, "USER-WHY", 'bareword .WHY calls the user override on the type object';
is Foo.'WHY'(), "USER-WHY", "quoted .'WHY'() calls the user override on the type object";
is Foo."$m2"(), "USER-WHY", 'dynamic ."$m"() calls the user override on the type object';

# A named-variable (mut) receiver reaches the same override.
my $obj = Foo.new;
is $obj.WHICH, "USER-WHICH", 'bareword .WHICH on a named variable calls the user override';
is $obj.WHY, "USER-WHY", 'bareword .WHY on a named variable calls the user override';

done-testing;
