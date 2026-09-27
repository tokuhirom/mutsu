use Test;

# A statement-leading bare block `{ ... }` is a term, not an executed
# statement, when a built-in word infix immediately follows it on the same
# line: `{ ... } or die` never runs the block or `die` — the block is a
# (truthy) Block object, and `or`'s left operand is already true. mutsu's
# statement parser used to commit `{ ... }` to a bare-block statement
# unconditionally, so `or`/`and`/etc. were parsed as a bogus new statement
# instead (`Undeclared routine: or used`). Found by the 2026-09-27 doc-diff
# sweep (`Language/control.rakudoc:48`); GitHub issue #9781.

plan 7;

{
    my $ran = False;
    my $result = do {
        { $ran = True; 0; } or 99;
    };
    is $result.^name, 'Block', 'the bare block is a truthy Block term, not its executed value';
    ok !$ran, 'the block body never runs -- "or" short-circuits on the truthy Block';
}

{
    # The issue's own repro.
    my @log;
    @log.push('start');
    { @log.push('then'); };
    { @log.push('not-here'); 0; } or @log.push('die-branch');
    @log.push('done');
    is @log, ['start', 'then', 'done'], 'a bare block followed by "or" parses and short-circuits correctly';
}

{
    my $ran = False;
    my $result = do {
        { $ran = True; 0; } and 99;
    };
    is $result, 99, '"and" evaluates its right side because the Block term is truthy';
    ok !$ran, 'the block body still never runs under "and"';
}

{
    my @log;
    sub logit($n) { @log.push($n) }
    { @log.push('b') } logit(1);
    is @log, ['b', 1], 'a bare block followed by an ordinary call is unaffected -- still two statements';
}

{
    my $x = 0;
    { $x++ }
    is $x, 1, 'a bare block on its own line still runs as an executed statement';
}

done-testing;
