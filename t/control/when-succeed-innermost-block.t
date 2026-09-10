use v6;
use Test;

plan 20;

# A `when`/`default` succeed exits its INNERMOST enclosing block only — not an
# outer `given`/`with`/`for`. DBIish's execute() computes each bind buffer with
# `do { when Blob {...} when Str {...} ... }` and then fills a MYSQL_BIND in a
# following `given` block; when the succeed leaked out of the `do`, the whole
# rest of the `with` body was skipped and every inserted value became NULL.

{
    my @out;
    for 'a', 42 -> $v {
        with $v {
            my $x = do {
                when Int { 'int' }
                when Str { 'str' }
                default  { 'other' }
            };
            @out.push($x);
        }
    }
    is @out.join('|'), 'str|int',
        'a matched when inside do {} exits only the do block';
}

{
    my $x = do given 42 { 'seed' };
    given 42 {
        $x = do { when Int { 'i' }; default { 'd' } };
    }
    is $x, 'i', 'the do block yields the matched when body value';
}

{
    my $reached = False;
    given 42 {
        my $x = do { when Int { 'i' } };
        $reached = True;
    }
    ok $reached, 'the given body continues after an inner do-when match';
}

{
    my $tail = False;
    given 5 {
        when Int { }
        $tail = True;
    }
    nok $tail, 'a when directly inside given still exits the given';
}

{
    my $tail = False;
    with 5 {
        when Int { }
        $tail = True;
    }
    nok $tail, 'a when directly inside with still exits the with';
}

{
    my @t;
    for 1, 2 {
        when 1 { @t.push('one') }
        @t.push("tail$_");
    }
    is @t.join('|'), 'one|tail2',
        'a when in a for body ends the iteration, not the loop';
}

# The innermost enclosing block is the barrier even when it is an `if` branch, a
# bare block or a loop body that the compiler would otherwise emit as a plain
# jump — the succeed must not travel on to the enclosing `given`.

{
    my @log;
    given 5 { if True { when Int { @log.push('in') } }; @log.push('after') }
    is @log.join('|'), 'in|after', 'a when in an if branch does not escape the given';
}

{
    my @log;
    given 5 { if True { when Str { @log.push('in') } }; @log.push('after') }
    is @log.join('|'), 'after', 'a non-matching when in an if branch changes nothing';
}

{
    my @log;
    given 5 { unless False { when Int { @log.push('in') } }; @log.push('after') }
    is @log.join('|'), 'in|after', 'a when in an unless branch does not escape the given';
}

{
    my @log;
    given 5 {
        if False { @log.push('no') } elsif True { when Int { @log.push('in') } }
        @log.push('after');
    }
    is @log.join('|'), 'in|after', 'a when in an elsif branch does not escape the given';
}

{
    my @log;
    given 5 { if True { default { @log.push('d') } }; @log.push('after') }
    is @log.join('|'), 'd|after', 'a default in an if branch does not escape the given';
}

{
    my @log;
    given 5 {
        if True { if True { when Int { @log.push('deep') } }; @log.push('mid') }
        @log.push('after');
    }
    is @log.join('|'), 'deep|mid|after',
        'the succeed stops at the innermost branch, not the outer one';
}

{
    my @log;
    given 5 { { when Int { @log.push('a') }; @log.push('unreached') }; @log.push('after') }
    is @log.join('|'), 'a|after', 'a when in a bare block ends only that block';
}

{
    my @log;
    given 5 { { when Int { @log.push('a') } }; { when Str { @log.push('b') } }; @log.push('after') }
    is @log.join('|'), 'a|after', 'a later bare block still runs after the first succeeded';
}

# A block that contains no `when` is NOT a barrier: an explicit `succeed` inside
# it travels out to the enclosing topicalizer.
{
    my @log;
    given 5 {
        when Int { { @log.push('a'); succeed; @log.push('b') }; @log.push('c') }
        @log.push('d');
    }
    @log.push('e');
    is @log.join('|'), 'a|e',
        'an explicit succeed in a when-less block still exits the given';
}

{
    my @log;
    given 5 { for 1, 2 { when Int { @log.push('m') } }; @log.push('after') }
    is @log.join('|'), 'm|m|after', 'a when in a for body only ends the iteration';
}

{
    my @log;
    my $i = 0;
    given 5 { while $i < 2 { $i++; when Int { @log.push('m') } }; @log.push('after') }
    is @log.join('|'), 'm|m|after', 'a when in a while body only ends the iteration';
}

{
    my @log;
    given 5 { loop (my $j = 0; $j < 2; $j++) { when Int { @log.push('L') } }; @log.push('after') }
    is @log.join('|'), 'L|L|after', 'a when in a C-style loop body only ends the iteration';
}

{
    my @log;
    given 5 { repeat { when Int { @log.push('R') } } while False; @log.push('after') }
    is @log.join('|'), 'R|after', 'a when in a repeat body only ends the iteration';
}

{
    my @log;
    for 1, 2 { { when 1 { @log.push('one') } }; @log.push('tail') }
    is @log.join('|'), 'one|tail|tail',
        'a bare block absorbs the succeed inside a for loop';
}
