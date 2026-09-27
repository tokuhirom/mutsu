use Test;

# When a coercion (`my C1(Any) $v; $v = Bar.new(...)`) has no matching
# `COERCE` candidate and falls back to calling `new` on the target class,
# that `new` candidate runs with the dynamic variable `$*COERCION-TYPE` bound
# to the coercion's target type -- see roast S12-coercion/coercion-methods.t's
# "method new has its context set" subtest.

plan 2;

class Bar {
    has $.value;
}

class Target {
    has $.value;
    has $.seen-coercion-type;
    multi method new(::?CLASS:U: Bar:D $bar) {
        self.new: :value($bar.value), :seen-coercion-type($*COERCION-TYPE);
    }
}

my Target(Any) $v;
$v = Bar.new(:value("ok"));

is $v.value, "ok", "coercion fell back to the custom new candidate";
# $*COERCION-TYPE holds the Target type OBJECT, so it is legitimately
# undefined (a type object is never .defined) -- isa-ok is the right check.
isa-ok $v.seen-coercion-type, Target, '$*COERCION-TYPE held the coercion target type';
