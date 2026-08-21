use Test;

# `my %h is CustomTrait = initializer` where `CustomTrait` is a
# user-declared *trait name* dispatched through
# `multi sub trait_mod:<is>(Variable:D \v, ...)` (NOT itself a registered
# class/role name) that mixes in a role with a `STORE` method must still see
# the declaration's own initializer through that `STORE`, with `:INITIALIZE`
# set — the same way a directly-named `is ClassName` tie already does (see
# t/tied-hash-store-initialize.t). Regression test for the ordering bug
# where the compiler's plain `SetLocal` (the raw initializer store) always
# ran before the `is`-trait's role mixin, so a role mixed in indirectly via
# a custom trait handler never saw its own declaration-time `STORE` at all.

plan 3;

subtest 'custom trait_mod:<is> mixing in a STORE-bearing role sees the initializer' => {
    plan 3;

    my @seen;

    my role R {
        method STORE(\to_store, :$INITIALIZE) {
            @seen.push((to_store.list.sort(*.key), $INITIALIZE.so));
            callsame;
        }
    }
    multi sub trait_mod:<is>(Variable:D \v, Bool:D :$restricted!) {
        trait_mod:<does>(v, R) if $restricted;
    }

    my %h is restricted = a => 42, b => 666;

    is +@seen, 1, 'STORE ran exactly once for the declaration';
    is-deeply @seen[0][0]».key, ('a', 'b'), 'STORE saw the initializer pairs';
    ok @seen[0][1], 'STORE was called with :INITIALIZE true';
}

subtest 'is ClassName (a registered role/class named directly) still works' => {
    plan 2;

    my @seen;

    my class Tied does Associative {
        has %.store;
        method STORE(\values, :$INITIALIZE) {
            @seen.push($INITIALIZE ?? 'init' !! 'reassign');
            %!store = ();
            for values.list -> $p { %!store{$p.key} = $p.value }
            self
        }
        method AT-KEY($k) { %!store{$k} }
    }

    my %h is Tied = x => 1, y => 2;
    is %h<x>, 1, 'declaration initializer still stored via is ClassName';
    is-deeply @seen, ['init'], 'is ClassName STORE still gets :INITIALIZE';
}

subtest 'a custom trait mixing in a role WITHOUT STORE is unaffected' => {
    plan 2;

    my role Plain {
        method greeting { 'hi' }
    }
    multi sub trait_mod:<is>(Variable:D \v, Bool:D :$plain-tagged!) {
        trait_mod:<does>(v, Plain) if $plain-tagged;
    }

    my %h is plain-tagged = a => 1, b => 2;
    is %h<a>, 1, 'initializer stored normally with no STORE to re-feed';
    is %h.greeting, 'hi', 'the mixed-in role is still usable';
}
