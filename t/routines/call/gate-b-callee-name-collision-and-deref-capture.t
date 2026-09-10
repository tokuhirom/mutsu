use Test;

# Pin for two (B) per-store env-write gate fixes
# (docs/lexical-scope-slot-campaign.md, "(B) per-store env-write gate — burndown").
#
# 1. Bare call vs. same-named local ("e.t"): a scalar `$e` is stored under the
#    bare name `e`, so `my $e = e; e()` collides — the function-call fallback
#    reads `env[e]` to decide whether `e()` is a bound-generic-type-parameter
#    coercion (`T()` -> `Int(Any)`). Under the gate a plain `my $e` keeps only
#    its decl-seed `Any` in env (its initializing store's mirror is skipped), so
#    the fallback saw `Package(Any)` and returned `Any(Any)` instead of dying
#    with X::Undeclared. The fix folds every such colliding local back into
#    `needs_env_sync` (compute_needs_env_sync) AND seeds throws-like's nested
#    interpreter from the caller frame's LIVE slot values.
#
# 2. `@$s` / `%$s` deref capture ("skip.t"): a `@$s` deref records its closure
#    free var as `@s`, but the underlying lexical is the sigil-less scalar `s`,
#    so the closure-capture env-sync fold missed it. Under the gate a
#    `throws-like { @$s }` block read a stale (non-consumed) `$s` from env. The
#    fix strips the sigil when matching the free var against this frame's locals.
#
# Gate OFF (default) is byte-identical (both folds are gate-ON only). These pass
# gate-OFF and would fail gate-ON before the fix.

plan 6;

# --- 1. bare call colliding with a same-named local (roast S32-trig/e.t) ---
my $e = e;
throws-like 'e()', X::Undeclared, 'e() with a same-named local $e still dies';
is $e, e, '$e still holds the constant e';

# --- 2. @$s / %$s deref capture in a throws-like block ---
{
    my $s := (1..3).Seq;
    my $skipped := $s.skip;
    throws-like { @$s }, X::Seq::Consumed, 'consumed Seq via @$s deref in a block';
    is-deeply @$skipped, (2, 3), 'skipped has right content';
}

# The @$s deref inside a plain closure sees the live bound container.
{
    my $a := [10, 20, 30];
    my &b = { @$a };
    is-deeply b().List, (10, 20, 30), '@$a deref closure captures the live array';
}

# %$h deref inside a closure sees the live bound hash.
{
    my $h := {a => 1, b => 2};
    my &b = { (%$h){'a'} };
    is b(), 1, '%$h deref closure captures the live hash';
}
