use Test;

plan 6;

# A forward-referenced `&`-sigil lexical: an outer closure reads `&f` BARE
# (as a value, not a call) before the enclosing `my &f = ...` declaration has
# even compiled, let alone run. mutsu used to treat every bare `&name` read
# as an eagerly-captured free variable regardless of whether the enclosing
# scope had declared it yet, so the outer closure froze `&f`'s
# pre-declaration local slot (still Nil at that point) into its own captured
# env forever. Real Raku resolves the reference live, once the later
# assignment has run. This is the `&`-sigil twin of the value-typed-scalar
# snapshot bug fixed for captured `my int` / `Int:D is rw` locals.
#
# NOTE: each subtest below uses its own unique sub/lexical names rather than
# reusing e.g. `&f`/`&g`/`outer` across separate top-level blocks. Reusing
# the same `&`-sigil names across sibling blocks hits a SEPARATE, pre-existing
# compiler bug (`outer_code_var_names`/`local_map` leaking a declaration
# across sibling block scopes instead of resetting at the block boundary,
# affecting bare-call forward references too — see
# todo/tickets/sibling-block-code-var-name-leak.md) that is unrelated to this
# fix; using unique names keeps this file a clean pin of the forward-capture
# fix alone.

{
    # Minimal repro from the bug ticket: `&f` is forward-DECLARED first, then
    # a sibling closure captures a bare call `f()` to it, then `&f` is
    # ASSIGNED (not merely declared). The call form was never affected by
    # this bug (a bare call's callee name is looked up dynamically), but it
    # pins the ticket's own repro shape.
    my &f1;
    my &g1 = -> { f1() };
    &f1 = -> { 42 };
    is g1(), 42, 'call-form forward reference through a pre-declared &f resolves after assignment';
}

{
    # The actual bug: a BARE `&f` VALUE read (not a call) inside a closure
    # created before `my &f = ...` has even been declared (not just
    # assigned) in the enclosing scope. `.()` invokes the captured value
    # directly, without re-binding it to yet another lexical.
    sub fwd-bare-read {
        my &g2 = -> { &f2.(21) };
        my &f2 = -> $x { $x * 2 };
        g2();
    }
    is fwd-bare-read(), 42, 'bare &f value read resolves after a later my &f declaration';
}

{
    # The same bare-&-read forward reference used as a CALLABLE ARGUMENT
    # (`.map(&f)`) — the exact shape CBOR::Simple's `decode-array` uses
    # (`.map(&decode)`), where `&decode` isn't declared until later.
    sub fwd-bare-map-arg {
        my &g3 = -> { (^3).map(&f3).List };
        my &f3 = -> $x { $x * 3 };
        g3();
    }
    is-deeply fwd-bare-map-arg(), (0, 3, 6),
        '&f passed bare as a .map argument resolves after a later declaration';
}

{
    # CBOR::Simple's actual mutual-recursion shape: several closures declared
    # first each reference a LATER closure `&decode` bare; `&decode` itself
    # dispatches back into the earlier closures by CALL (not bare read) —
    # only the forward (bare-read) direction needs this fix, the backward
    # (call) direction already worked.
    sub cbor-shape($n) {
        my &decode-array = -> @arr {
            @arr.map(&decode).List;
        };
        my &decode-pair = -> $x {
            decode-array([$x, $x]);
        };
        my &decode = -> $x {
            $x < 2 ?? $x + 100 !! decode-pair($x - 1);
        };
        decode($n);
    }
    is-deeply cbor-shape(1), 101,
        'mutually-recursive forward/backward closures (CBOR::Simple shape), base case';
    is-deeply cbor-shape(2), (101, 101),
        'mutually-recursive forward/backward closures (CBOR::Simple shape), recursive case';
}

{
    # A closure created AFTER `my &f` was already declared (not a forward
    # reference at all) must keep resolving eagerly/correctly — the fix must
    # not regress the ordinary, non-forward case.
    sub non-forward-read {
        my &f4 = -> $x { $x + 1 };
        my &g4 = -> { &f4.(9) };
        g4();
    }
    is non-forward-read(), 10, 'a non-forward bare &f read (declared before the closure) still resolves';
}

# vim: expandtab shiftwidth=4
