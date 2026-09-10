use v6;
use Test;

plan 18;

# `EXPR for LIST` where `EXPR` is a closure with an explicit signature uses
# that closure as the loop body, so the loop must consume as many elements per
# iteration as the closure's arity -- exactly like `for LIST -> SIG { ... }`
# does for the same signature written the other way round.

# --- multi-param pointy block, expression position ---------------------------

{
    my @r = (-> $a, $b { "$a/$b" } for 1, 2, 3, 4);
    is-deeply @r, ['1/2', '3/4'], 'a 2-param pointy block consumes 2 elements per iteration';
}

# --- multi-param pointy block, statement position -----------------------------

{
    my @r;
    -> $a, $b { @r.push("$a/$b") } for 1, 2, 3, 4;
    is-deeply @r, ['1/2', '3/4'], 'same, in statement position';
}

# --- 3-param arity ------------------------------------------------------------

{
    my @r = (-> $a, $b, $c { "$a-$b-$c" } for 1, 2, 3, 4, 5, 6);
    is-deeply @r, ['1-2-3', '4-5-6'], 'a 3-param pointy block consumes 3 elements per iteration';
}

{
    # A final chunk shorter than the required arity dies mid-loop (after the
    # full chunks already ran), matching raku's batching semantics.
    try { (-> $a, $b, $c { "$a-$b-$c" } for 1, 2, 3, 4, 5) };
    ok $!, 'a short final chunk (5 elements, arity 3) dies';
    ok $!.message.contains('expected 3'), 'the error reports the required arity';
}

{
    try { (-> $a, $b, $c { "$a-$b-$c" } for 1, 2, 3, 4) };
    ok $!, 'a lone leftover element (4 elements, arity 3) also dies';
    ok $!.message.contains('expected 3'), 'same required-arity message';
}

# --- an optional (defaulted) trailing param -----------------------------------

{
    my @r = (-> $a, $b = 9 { "$a/$b" } for 1, 2, 3, 4);
    is-deeply @r, ['1/2', '3/4'], 'an even count fills the optional param from the source';
}

{
    my @r = (-> $a, $b = 9 { "$a/$b" } for 1, 2, 3);
    is-deeply @r, ['1/2', '3/9'], 'a short final chunk falls back to the default value';
}

# --- `sub (...) { ... }` behaves the same as a pointy block ------------------

{
    my @r = (sub ($a, $b) { "$a+$b" } for 1, 2, 3, 4);
    is-deeply @r, ['1+2', '3+4'], 'an anonymous sub with 2 params consumes 2 elements per iteration';
}

# --- an explicit slurpy-only signature ----------------------------------------

{
    my @r = (-> *@a { "@a[]" } for 1, 2, 3, 4);
    is-deeply @r, ['1', '2', '3', '4'], 'a bare slurpy pointy block still consumes one element per iteration';
}

# --- `<->` (rw) multi-param pointy block writes back to the source -----------

{
    my @a = (1, 2);
    (<-> $a, $b { $a += 10; $b += 100 } for @a);
    is-deeply @a, [11, 102], 'an rw multi-param pointy block writes back through its params';
}

# --- must NOT regress: cases that stay one element at a time -----------------

{
    # A single-param pointy block is already arity 1; unaffected by this change.
    my @r = (-> $x { $x + 1 } for 1, 2, 3);
    is-deeply @r, [2, 3, 4], 'a single-param pointy block still consumes one element per iteration';
}

{
    # WhateverCode is always arity 1, no matter how many `*` placeholders occur
    # in the expression -- it must NOT become the loop's own signature.
    my @a = (1, 2, 3);
    my @r = (* + 1 for @a);
    is-deeply @r, [2, 3, 4], 'WhateverCode stays one element at a time';
}

{
    # The implicit `@_` bare-block form is invoked one element at a time in
    # rakudo even though its only parameter is a synthesized slurpy `*@_`.
    my @r = ({ "@_[]" } for 1, 2, 3, 4);
    is-deeply @r, ['1', '2', '3', '4'], 'the implicit @_ bare block stays one element at a time';
}

{
    # A bare block's placeholder parameters (fixed separately) must not regress.
    my @r;
    { @r.push($^a ~ '/' ~ $^b) } for (1, 2), (3, 4);
    is-deeply @r, ['1 2/3 4'], 'placeholder-parameter bare block still consumes two elements';
}

{
    # A reference to a named sub (`&foo`) is a plain value, not a closure
    # literal -- it is collected once per topic element, never invoked.
    my $called = False;
    sub named-callee($a, $b) { $called = True; "$a|$b" }
    my @r = (&named-callee for 1, 2, 3, 4);
    is @r.elems, 4, 'a named-sub reference is collected once per element';
    ok !$called, 'and is never invoked';
}
