use v6;
use Test;

# When an object is created with a runtime role mixin (`C.new but role {...}`,
# or `(C but role {...}).new`), a method inherited from the class/base role must
# run with `self` bound to the MIXED object — so a nested `self.foo` inside that
# inherited method re-dispatches through the mixin roles and finds their
# overrides, rather than resolving against the bare inner instance.
#
# This is the shape of zef's recommendation-manager mock:
# `(Zef::Repository but role { method plugins {...} }).new(:backends[])`, whose
# inherited `Zef::Repository.candidates` calls `self.plugins` and must reach the
# mixin's `plugins`, not the base `Pluggable.plugins`.

plan 8;

# --- (C but role).new: inherited method's self.foo hits the mixin override ---
{
    role R {
        method who() { "BASE" }
        method call-who() { self.who }
    }
    class C does R { }
    my $obj = (C but role :: { method who() { "OVERRIDE" } }).new;
    is $obj.who, "OVERRIDE", "direct call hits mixin override";
    is $obj.call-who, "OVERRIDE", "inherited method self-dispatch hits mixin override";
}

# --- C.new but role: same, via the postfix mixin form ---
{
    class D {
        method label() { "D-base" }
        method describe() { "label=" ~ self.label }
    }
    my $obj = D.new but role :: { method label() { "D-mixed" } };
    is $obj.label, "D-mixed", "postfix-but direct override";
    is $obj.describe, "label=D-mixed", "postfix-but inherited self-dispatch override";
}

# --- attribute mutation via an inherited method still persists (self=Mixin) ---
{
    role Acc {
        has @.items;
        method add($x) { @!items.push($x) }
        method count() { @!items.elems }
    }
    class E does Acc { }
    my $obj = (E but role :: { method tag() { "T" } }).new;
    $obj.add(10);
    $obj.add(20);
    is $obj.count, 2, "attr mutation through inherited method persists under mixin";
    is $obj.tag, "T", "mixin method coexists with inherited attr-mutating methods";
}

# --- multi-level self-dispatch: inherited -> inherited -> override ---
{
    role Chain {
        method a() { self.b }
        method b() { self.c }
        method c() { "chain-BASE" }
    }
    class F does Chain { }
    my $obj = (F but role :: { method c() { "chain-OVERRIDE" } }).new;
    is $obj.a, "chain-OVERRIDE", "multi-hop self-dispatch reaches mixin override";
}

# --- inherited method taking args, nested self-dispatch with args ---
{
    role G {
        method scale($n) { self.base-value * $n }
        method base-value() { 1 }
    }
    class H does G { }
    my $obj = (H but role :: { method base-value() { 10 } }).new;
    is $obj.scale(3), 30, "inherited method with args reaches mixin override via self";
}
