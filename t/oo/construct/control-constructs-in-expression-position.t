use Test;

plan 19;

# --- `when` / `default` as a TERM inside an expression -----------------------
# Raku's `(...)` holds a semilist of *statements*, so a control clause is a
# legal term there. A clause that matches runs its block and then unwinds the
# enclosing topicalizer via `succeed`, exactly like the statement form.

{
    my @log;
    given 42 {
        @log.push: 'a';
        $_ == 42 and ( default { @log.push: 'b'; 43 } );
        @log.push: 'c';
    }
    is-deeply @log, ['a', 'b'],
        'a `default` term nested in an expression exits the enclosing given';
}

{
    my $seen;
    given 1 {
        $seen = ( when 42 { 43 } );
        $seen ~= '/after';
    }
    is $seen, 'False/after', 'a non-matching `when` term evaluates to False';
}

{
    my @log;
    given 42 {
        @log.push: ( when 42 { 'matched' } );
        @log.push: 'not reached';
    }
    is-deeply @log, [], 'a matching `when` term abandons the enclosing given';
}

{
    sub only-default { default { 7 } }
    is only-default(), 7, '`default` as the sole statement of a sub yields its block value';
}

# --- `proceed` is transparent to a statement-modifier `when` -----------------
# `STMT when COND` is not a `when` *clause*: Rakudo lowers it to a plain
# conditional, so a `proceed` raised inside it keeps unwinding to the nearest
# real `when` clause instead of being consumed by the modifier.

{
    my @log;
    given 42 {
        when * > 41 {
            { @log.push: 'A'; proceed } when * > 41;
            @log.push: 'B';
        }
    }
    is-deeply @log, ['A'],
        '`proceed` inside a modifier-`when` unwinds the enclosing `when` clause';
}

{
    my @log;
    given 42 {
        when * > 41 {
            { @log.push: 'A'; proceed };
            @log.push: 'B';
        }
        when * > 40 { @log.push: 'C' }
    }
    is-deeply @log, ['A', 'C'],
        '`proceed` from a nested bare block continues with the next `when` clause';
}

{
    my @log;
    given 42 {
        @log.push: 'A' when * > 41;
        @log.push: 'B';
    }
    is-deeply @log, ['A', 'B'],
        'a matching modifier-`when` does NOT abandon the enclosing block';
}

{
    my @log;
    for 1, 2 {
        @log.push: "x$_" when * > 1;
        @log.push: "y$_";
    }
    is-deeply @log, ['y1', 'x2', 'y2'],
        'a modifier-`when` in a loop body runs the rest of the iteration';
}

# --- statement modifiers inside a parenthesized comma list ------------------
# The modified statement is the whole preceding comma expression and the
# modifier's condition is itself a full comma expression.

{
    my $got = (1, 2 if True, 3);
    is $got.elems, 2, 'a mid-list `if` modifier keeps only the preceding comma list';
    is $got.join(','), '1,2', '... with the expected elements';
}

{
    my $got = (1, 2 unless False, 3);
    is $got.elems, 0,
        'the `unless` condition is the whole comma list `(False, 3)`, which is truthy';
}

{
    my $got = (1 if True, 2, 3);
    is $got.join(','), '1', 'a single-item statement with a comma-list condition';
}

{
    my $got = (1, 2, 3 if True);
    is $got.join(','), '1,2,3', 'a trailing `if` modifier over a three-item list';
}

{
    my $x = 0;
    $x = 1 if False, 3;
    is $x, 1, 'a statement-level `if` condition absorbs the trailing comma list';
}

# --- `race for` in value-collecting position --------------------------------
# `race` does not guarantee order, so assert order-insensitively.

{
    my $r = race for ^10 -> $n { $n if $n %% 2 };
    is $r.elems, 5, '`race for` collects one value per truthy iteration';
    is $r.sort.join(','), '0,2,4,6,8', '... and collects the right values';
}

# --- `take` raises a CONTROL-catchable CX::Take -----------------------------

{
    my @seen;
    my @taken = gather {
        CONTROL { when CX::Take { @seen.push: 'take'; .resume } }
        take 1;
        take 2;
    };
    is @seen.elems, 2, 'a `CONTROL { when CX::Take }` sees every `take` in the gather';
    is @taken.elems, 0, '... and a resumed CX::Take discards the taken value';
}

{
    my @taken = gather {
        CONTROL { when CX::Warn { .resume } }
        take 1;
        take 2;
    };
    is-deeply @taken, [1, 2],
        'a CONTROL block that cannot match CX::Take leaves `take` alone';
}
