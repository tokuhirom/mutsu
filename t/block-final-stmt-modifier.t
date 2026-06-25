use Test;

# A statement modifier (if/unless/while/until/given) never takes a block.
# `if COND { ... }` after a block-final statement on the previous line is a
# full control statement, not a postfix modifier attached to the prior line.

plan 8;

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

# Regression (the inverse): a NON-block statement modified by `if` whose
# CONDITION ends in a block, followed by a separate `{*}` block on the next
# line. `die X if %h.keys.first: { ... }` then `{*}` — the `if` IS a modifier
# (die does not end in a block) and the trailing `{*}` is a separate statement
# (the proto dispatch). The block-after-condition guard must only fire when the
# *modified* statement itself ends in a block. (HTTP::Tiny's `proto method
# request` uses exactly this shape.)
{
    class P1 {
        proto method req(:%headers, |c) {
            die "Host not allowed"
                if %headers.keys.first: { .defined && m:i/ ^ host $ / }
            {*}
        }
        multi method req(|c) { 'ran' }
    }
    is P1.req, 'ran',
        'proto die-guard does not mis-fire when no matching header (modifier attaches to die)';
}

# The modifier still fires when its block-valued condition is true.
{
    my @keys = 'Host';
    my $fired = 0;
    $fired = 1
        if @keys.first: { .defined && m:i/ ^ host $ / }
    { ; }   # a separate bare block, like the proto's `{*}`
    is $fired, 1, 'die-style modifier with a true block-valued condition fires';
}

# The original protect-style case still self-terminates (`{` after the
# CONDITION of an `if` whose *modified* statement ends in a block).
{
    my $lock = Lock.new;
    my $x = 1;
    $lock.protect: { $x = 7 }
    if $x > 100 { $x = -1 }
    is $x, 7, 'block-final statement still self-terminates before a following if-statement';
}
