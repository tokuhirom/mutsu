use v6;
use Test;

plan 6;

# Pin the native-constructor plan cache invalidation (native_ctor_plan_cache):
# a class-shape mutation AFTER a construction already cached the class's plan
# must be visible to the next construction.

# Attribute.set_build after a construction (drops eligibility -> interpreter
# path applies the override). NOTE: rakudo ignores a post-first-`.new`
# set_build (its BUILDPLAN is cached at first construction); mutsu applies it
# dynamically — pre-existing, deliberately pinned divergence.
{
    class B { has $.v = 1 }
    is B.new.v, 1, "construction before set_build uses the default";
    B.^attributes.head.set_build(-> |c { 99 });
    is B.new.v, 99, "construction after set_build applies the override";
}

# augment class after a construction: the added method is callable on a
# fresh instance (the cached plan for the class is dropped, not stale).
{
    use MONKEY-TYPING;
    class C { has $.x }
    is C.new(x => 1).x, 1, "construction before augment";
    augment class C { method double() { $.x * 2 } }
    my $c = C.new(x => 21);
    is $c.x, 21, "construction after augment still binds named args";
    is $c.double, 42, "augment-added method works on a post-augment instance";
}

# Same-named class redeclared via EVAL (registry re-registration) is not
# served a stale plan from the earlier class of the same name.
{
    my $first = EVAL 'class D { has $.a }; D.new(a => 5).a';
    my $second = EVAL 'class D { has $.a; has $.b = 7 }; my $d = D.new(a => 1); $d.a + $d.b';
    is $first + $second, 13, "redeclared same-named class constructs with its own shape";
}

done-testing;
