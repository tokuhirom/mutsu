use v6;
use Test;

# The fast-dispatch method cache (`try_populate_fast_cache`,
# vm_call_method_compiled_cache.rs) is keyed by (class, method) only, and once
# populated on a cache MISS (the first call), later HITs skip the attribute
# write-back that an attributive parameter (`$!x`/`@!a`) needs (that
# write-back only runs in the slow `call_compiled_method` path, guarded by a
# `has_complex_params` check the cache-population twin was missing). A
# receiver read from a plain named variable never consults the fast cache
# (it compiles to `CallMethodMut`), so the bug only shows when the receiver
# is a fresh expression each call (array/hash subscript, a sub call) that
# resolves to a DIFFERENT instance on the second+ call.
#
# Symptom: the first call (a cache miss) correctly stores its argument into
# the attribute; every subsequent call (a cache hit, on a different receiver
# instance) silently drops the argument, leaving the attribute at its
# declaration-time seed (an undefined type object for a typed attribute,
# `Any` otherwise) instead of raising or storing the passed value.

plan 8;

class Box {
    has $!s;
    method set-s($!s --> Nil) { }
    method get-s() { $!s }
}

# Array-subscript receiver, three distinct instances: found via
# Cro::HTTP2::GeneralParser's `%streams{$curr-sid}.message.set-body-byte-
# stream($body.Supply)` shape (todo/deep/http2-concurrent-streams-first-
# body-blob-loses-value.md).
{
    my @b = (Box.new, Box.new, Box.new);
    @b[0].set-s(111);
    @b[1].set-s(222);
    @b[2].set-s(333);
    is @b[0].get-s, 111, "array-subscript receiver: 1st call (cache miss) writes back";
    is @b[1].get-s, 222, "array-subscript receiver: 2nd call (cache hit) writes back";
    is @b[2].get-s, 333, "array-subscript receiver: 3rd call (cache hit) writes back";
}

# Hash-subscript receiver.
{
    my %h;
    %h<a> = Box.new;
    %h<b> = Box.new;
    %h<a>.set-s(1);
    %h<b>.set-s(2);
    is %h<a>.get-s, 1, "hash-subscript receiver: 1st call writes back";
    is %h<b>.get-s, 2, "hash-subscript receiver: 2nd call writes back";
}

# Sub-call receiver.
{
    my $b1 = Box.new;
    my $b2 = Box.new;
    sub get($n) { $n == 1 ?? $b1 !! $b2 }
    get(1).set-s(11);
    get(2).set-s(22);
    is get(1).get-s, 11, "sub-call receiver: 1st call writes back";
    is get(2).get-s, 22, "sub-call receiver: 2nd call writes back";
}

# A typed attributive parameter: the declaration-time seed is the type
# object, not Any, so a dropped write is otherwise invisible under `.defined`
# checks that treat both as falsy — assert the type name too.
{
    class TypedBox {
        has Int $!n;
        method set-n(Int $!n --> Nil) { }
        method get-n() { $!n }
    }
    my @t = (TypedBox.new, TypedBox.new);
    @t[0].set-n(1);
    @t[1].set-n(2);
    is @t[1].get-n, 2, "typed attributive param: 2nd call (cache hit) writes back";
}
