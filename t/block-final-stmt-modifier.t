use Test;

# A statement modifier (if/unless/while/until/given) never takes a block.
# `if COND { ... }` after a block-final statement on the previous line is a
# full control statement, not a postfix modifier attached to the prior line.

plan 5;

# Same-line modifier still works (ends in `;`, not a block).
my @a = 1, 2, 3;
my $n = 0;
@a.map: { $n += $_ } if True;
is $n, 6, 'same-line block-call + postfix if still attaches';

{
    my @log;
    my $cond = False;
    do { @log.push('body') }
    if $cond { @log.push('if-branch') }
    is @log.join(','), 'body', 'do-block then if-statement parse as two statements';
}

{
    my $x = 5;
    my $hit = 0;
    do { $x = 10 }
    unless $x > 100 { $hit = 1 }
    is $hit, 1, 'unless after a block-final statement is its own statement';
}

{
    my $lock = Lock.new;
    my $count = 0;
    $lock.protect: { $count = 1 }
    until $count >= 3 { $count++ }
    is $count, 3, 'until-statement after block-final statement is separate';
}

{
    my $out = '';
    my @a = 'a';
    @a.map: { $out = $_ }
    given 3 { $out ~= "-$_" }
    is $out, 'a-3', 'given-statement after block-final statement is separate';
}
