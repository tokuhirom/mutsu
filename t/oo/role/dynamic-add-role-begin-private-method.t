use Test;

plan 4;

# A class that dynamically composes a role via `.^add_role()` inside its own
# `BEGIN` block, and then calls one of that role's PRIVATE methods from
# another method in the same class body. This is the pattern
# AttrX::Mooish::X::StoreValue uses to compose the AttrX::Mooish::X::Wrapper
# role (github.com/tokuhirom/mutsu#8573): the class-registration shell pass
# mutsu runs to support forward references does not run `BEGIN` blocks (it
# never executes body statements at all), so it does not yet see the role's
# private methods -- validating private-method calls against that
# incomplete shell used to raise a spurious X::Method::NotFound and abort
# the whole class registration before the real, BEGIN-completed pass ever
# ran.

role Greeter {
    method !greeting(:$loud) {
        $loud ?? "HELLO" !! "hello"
    }
}

class Talker {
BEGIN {
    ::?CLASS.^add_role(::('Greeter'));
}
    method say-hi { self!greeting }
    method shout-hi { self!greeting(:loud) }
}

is Talker.say-hi, "hello", "private method from a BEGIN-composed role resolves";
is Talker.shout-hi, "HELLO", "named args reach a BEGIN-composed role's private method";

# Same shape, but the role is referenced through a compound (`::`-qualified)
# indirect name, matching AttrX::Mooish::X::Wrapper's own
# `::?CLASS.^add_role(::('X::Wrapper'))` spelling more closely.
role NS::Greeter2 {
    method !greeting2 { "hi there" }
}

class Talker2 {
BEGIN {
    ::?CLASS.^add_role(::('NS::Greeter2'));
}
    method say-hi { self!greeting2 }
}

is Talker2.say-hi, "hi there", "private method from a compound-named BEGIN-composed role resolves";
ok Talker2 ~~ NS::Greeter2, "the dynamically added role is actually composed";
