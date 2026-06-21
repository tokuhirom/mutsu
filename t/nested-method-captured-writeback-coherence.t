use Test;

# Slice 1b (env<->locals coherence): a `method` declared inside another method's
# body captures an outer lexical and writes it when later called. The nested method
# is dispatched through the interpreter `call_sub_value` path (not the compiled
# method merge path), so its captured-outer write must reconcile the caller's local
# slot via the precise `pending_rw_writeback_sources` drain — independent of the
# blanket reverse-sync pull. Each subtest must hold both with the blanket reconcile
# ON (default) and OFF (`MUTSU_NO_BLANKET_RECONCILE=1`).

plan 9;

# 1. Basic captured-outer scalar write via a nested method.
{
    my $tracker;
    class A1 {
        method foo {
            my $a = 42;
            method bar { $tracker = $a }
        }
    }
    given A1.new {
        .foo;
        .bar;
    }
    is $tracker, 42, 'nested method writes captured outer lexical (via given/topic)';
}

# 2. Explicit invocant call form.
{
    my $tracker;
    class A2 {
        method foo {
            method bar { $tracker = 77 }
        }
    }
    my $o = A2.new;
    $o.foo;
    $o.bar;
    is $tracker, 77, 'nested method writes captured outer (explicit invocant)';
}

# 3. Read-modify-write accumulation across repeated calls.
{
    my $count = 0;
    class A3 {
        method install {
            method tick { $count++ }
        }
    }
    my $o = A3.new;
    $o.install;
    $o.tick;
    $o.tick;
    $o.tick;
    is $count, 3, 'nested method RMW accumulates across calls';
}

# 4. += compound assignment to captured outer.
{
    my $sum = 10;
    class A4 {
        method setup {
            method add5 { $sum += 5 }
        }
    }
    my $o = A4.new;
    $o.setup;
    $o.add5;
    $o.add5;
    is $sum, 20, 'nested method += accumulates';
}

# 5. Captured outer string write.
{
    my $name = 'init';
    class A5 {
        method setup {
            method rename { $name = 'changed' }
        }
    }
    my $o = A5.new;
    $o.setup;
    $o.rename;
    is $name, 'changed', 'nested method writes captured outer string';
}

# 6. Both: nested method reads an enclosing-method lexical and writes an outer one.
{
    my $out;
    class A6 {
        method foo {
            my $secret = 99;
            method reveal { $out = $secret }
        }
    }
    my $o = A6.new;
    $o.foo;
    $o.reveal;
    is $out, 99, 'nested method reads enclosing lexical, writes outer';
}

# 7. Multiple captured-outer scalars written in one nested method.
{
    my $x = 0;
    my $y = 0;
    class A7 {
        method setup {
            method bump { $x = 1; $y = 2 }
        }
    }
    my $o = A7.new;
    $o.setup;
    $o.bump;
    is "$x,$y", '1,2', 'nested method writes multiple captured outers';
}

# 8. Coherent in a later expression in the same scope.
{
    my $v;
    class A8 {
        method setup {
            method go { $v = 5 }
        }
    }
    my $o = A8.new;
    $o.setup;
    $o.go;
    my $doubled = $v * 2;
    is $doubled, 10, 'caller slot coherent in a later expression';
}

# 9. Captured outer survives an intervening unrelated method call.
{
    my $tracker;
    class A9 {
        method setup {
            method mark { $tracker = 'marked' }
        }
        method noop { 1 }
    }
    my $o = A9.new;
    $o.setup;
    $o.mark;
    $o.noop;
    is $tracker, 'marked', 'captured-outer write survives an intervening call';
}
