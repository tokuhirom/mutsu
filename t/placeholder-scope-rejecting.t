use Test;

plan 22;

# ADR-0048 Phase 2: constructs whose body may NOT take a signature.
# `raku` gives the exact same `X::Placeholder::Block` for all of these
# ("Placeholder variable '$^c' may not be used here because the surrounding
# block does not take a signature."), verified against real `raku` when this
# ADR was written (docs/adr/0048-placeholder-scope-is-a-block-invocation-contract.md,
# "Constructs whose body may not take a signature"). Each case here pins one
# row of that table.

throws-like 'loop { $^c; last }', X::Placeholder::Block,
    'loop {} (headerless) rejects a placeholder';

throws-like 'loop (my $i = 0; $i < 3; $i++) { $^c }', X::Placeholder::Block,
    'loop {} (C-style) rejects a placeholder';

# `repeat {} while/until` is NOT part of this phase's rejecting set -- unlike
# headerless/C-style `loop {}`, raku's own evidence table classifies it as
# signature-capable (Mu on the first pass, then the condition value; ADR-0048
# D4/Phase 4 implements the actual binding later). Pin the accepting behavior
# here so a future change does not accidentally lump it in with `loop {}` --
# `raku` itself does not reject this
# (roast/S04-statements/repeat.t's "placeholders and 'repeat while' mix").
{
    my $b = 1;
    my $tracker;
    repeat while $b < 10 {
        $tracker = $^a;
        $b++;
    }
    ok $tracker === True, 'repeat/while still accepts a placeholder (not part of this phase)';
}

throws-like 'try { $^c }', X::Placeholder::Block,
    'try {} rejects a placeholder';

throws-like 'react { $^c; done }', X::Placeholder::Block,
    'react {} rejects a placeholder';

throws-like 'once { $^c }', X::Placeholder::Block,
    'once {} rejects a placeholder';

throws-like 'given 1 { default { $^c } }', X::Placeholder::Block,
    'default {} rejects a placeholder';

throws-like 'try { die "x"; CATCH { $^c } }', X::Placeholder::Block,
    'standalone CATCH {} rejects a placeholder';

throws-like 'try { die "x"; CONTROL { $^c } }', X::Placeholder::Block,
    'standalone CONTROL {} rejects a placeholder';

throws-like 'BEGIN { $^c }', X::Placeholder::Block,
    'BEGIN {} rejects a placeholder (mainline, hoisted)';

throws-like 'sub f { BEGIN { $^c } }; f()', X::Placeholder::Block,
    'BEGIN {} rejects a placeholder (routine tail position)';

throws-like 'CHECK { $^c }', X::Placeholder::Block,
    'CHECK {} rejects a placeholder';

throws-like 'INIT { $^c }', X::Placeholder::Block,
    'INIT {} rejects a placeholder';

# PRE {}/POST {} at the true mainline are not currently enforced at all by
# mutsu (a pre-existing gap unrelated to ADR-0048 -- `PRE { False }` at the
# mainline does not die), so this pins the sub-body form instead, where PRE
# is enforced and reaches the same `compile_pre_phasers` primitive.
throws-like 'sub f { PRE { $^c }; 1 }; f()', X::Placeholder::Block,
    'PRE {} rejects a placeholder (sub body)';

throws-like 'my @r = gather { $^c; take 1 }', X::Placeholder::Block,
    'gather {} rejects a placeholder';

throws-like 'module M { $^c }', X::Placeholder::Block,
    'module {} rejects a placeholder';

throws-like 'package P { $^c }', X::Placeholder::Block,
    'package {} rejects a placeholder';

throws-like 'grammar G { $^c }', X::Placeholder::Block,
    'grammar {} rejects a placeholder';

throws-like 'supply { $^c }', X::Placeholder::Block,
    'supply {} rejects a placeholder';

throws-like 'start { $^c }', X::Placeholder::Block,
    'start {} rejects a placeholder';

throws-like 'sink { $^c }', X::Placeholder::Block,
    'sink {} rejects a placeholder';

throws-like 'my $x = lazy { $^c; 1 }; $x[0]', X::Placeholder::Block,
    'lazy {} rejects a placeholder';
