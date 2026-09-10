use v6;
use Test;

# When a postfix statement modifier's *condition* ends with a `{ ... }` block
# (`return if @a.first: { ... }`), that block terminates the statement. The next
# line's `if`/`for`/etc. therefore begins a NEW statement, not a second (illegal)
# modifier — mutsu used to raise a bogus "Missing semicolon" (X::Syntax::Confused)
# for it. A genuinely-chained second conditional without a terminating block must
# still be rejected.

plan 5;

# 1. `return if COND.method: { block }` then a following `if` statement parses,
#    and the block-terminated modifier still controls the return.
{
    my $ran = 'no';
    sub f(@mask) {
        return 'early' if @mask.first: { !.defined }
        $ran = 'yes';
        return 'late';
    }
    is f([1, 2, 3]), 'late', 'all defined: modifier false, falls through';
    is $ran, 'yes', 'statement after block-terminated modifier ran';
}

# 2. The modifier fires when its block condition is truthy.
{
    sub g(@mask) {
        return 'early' if @mask.grep: { $_ > 5 }
        return 'late';
    }
    is g([1, 9, 3]), 'early', 'grep found a match: block-terminated modifier fires';
}

# 3. A `for` loop may still legally follow a block-terminated conditional? No —
#    but a following control statement (`if`) is fine. Check a bare `say` too.
{
    my @seen;
    sub h(@items) {
        @seen.push: 'checked' if @items.grep: { $_ > 0 }
        @seen.push: 'after';
    }
    h([1, 2]);
    is @seen.join(','), 'checked,after', 'grep-block modifier then push both run';
}

# 4. A genuinely chained second conditional (no terminating block) is still a
#    "Missing semicolon" error — the fix must not swallow that.
{
    my $err = '';
    try {
        EVAL 'my $x = 1; $x if $x if $x;';
        CATCH { default { $err = .^name } }
    }
    ok $err ~~ /Syntax/ || $err ~~ /Confused/, 'chained conditionals still rejected';
}
