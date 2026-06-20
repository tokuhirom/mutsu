use Test;

# Slice F coherence pin: the method-call / function-call / say-note op tails
# reconcile the caller's local slots from env whenever the call dirtied env, so
# a captured-outer lexical mutated by user code run *inside* a call (without a
# callable argument) is coherent without the blanket reverse `sync_locals_from_env`
# pull. These shapes have no callable argument, so the older `args_have_callable`
# gate missed them. Run with MUTSU_NO_REVERSE_SYNC=1 to verify the write-through.

plan 8;

# 1. say() calls .gist on its argument, whose method mutates a captured-outer var.
{
    my $seen = 0;
    say "" but role { method gist() { $seen = 1; "" } };
    is $seen, 1, 'say() .gist captured-outer mutation is visible to caller';
}

# 2. note() likewise (writes to $*ERR, .gist side effect must still propagate).
{
    my $seen = 0;
    note "" but role { method gist() { $seen = 1; "" } };
    is $seen, 1, 'note() .gist captured-outer mutation is visible to caller';
}

# 3. A multi sub candidate body mutates a captured-outer var; no callable arg.
{
    my ($m1, $m2);
    multi sub pick(Int $) { $m1 = True }
    multi sub pick(Str $) { $m2 = True }
    pick(42);
    pick("x");
    ok $m1 && $m2, 'multi sub candidate bodies mutate captured-outer vars';
}

# 4. A plain sub call returning through an interpreter path, mutating an outer var.
{
    my $hit = 0;
    sub bump() { $hit++ }
    bump(); bump(); bump();
    is $hit, 3, 'plain sub captured-outer increment is visible to caller';
}

# 5. .Str method side effect via a stringifying builtin.
{
    my $called = 0;
    my $s = ("" but role { method Str() { $called = 1; "z" } }).Str;
    is $called, 1, '.Str captured-outer mutation is visible to caller';
}

# 6. say with multiple args, the side-effecting one in the middle.
{
    my $seen = 0;
    say 1, ("" but role { method gist() { $seen = 1; "" } }), 2;
    is $seen, 1, 'say() reaches a side-effecting .gist among several args';
}

# 7. map block (regression guard for the previous slice, still works).
{
    my $c = 0;
    (1, 2, 3).map({ $c++; $_ }).eager;
    is $c, 3, '.map captured-outer counter still coherent';
}

# 8. metaop thunk (regression guard for the previous slice, still works).
{
    my $s = 0;
    (($s++,),) Xxx 3;
    is $s, 3, 'X-metaop thunk captured-outer counter still coherent';
}
