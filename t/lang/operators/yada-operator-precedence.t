use Test;

# The stub listops `...`, `!!!` and `???` (`yada, yada, yada`, the fatal stub,
# and the admonitory stub) sit at "list prefix precedence" (operators.rakudoc:
# `listop C«...»` / `listop C«!!!»` / `listop C«???»`) — tighter than the loose
# word-logicals (`and`/`or`/...) and they may take no argument at all. mutsu
# used to parse their optional message with the full statement-level
# `expression()`, which swallowed a following statement modifier as a (bogus)
# argument expression, and bound looser than `or` instead of tighter (#9780).

plan 9;

# A bare stub followed directly by a statement modifier: no argument, the
# modifier decides whether the stub runs at all.
{
    my $x = False;
    ... if $x;
    pass('... if $x (false) parses and does not run');
}
{
    my $x = True;
    ??? if $x;
    pass('??? if $x (true) parses and runs without dying');
}
{
    my $x = True;
    ... unless $x;
    pass('... unless $x (true condition) parses and does not run');
}

# The same shape for `!!!`, which does die when it runs.
{
    my $x = True;
    try { !!! if $x; }
    is $!.message, 'Stub code executed', '!!! if $x (true) dies with the stub message';
}
{
    my $x = False;
    !!! if $x;
    pass('!!! if $x (false) parses and does not run');
}

# An explicit message argument still parses at this precedence (stops before
# the modifier, not absorbed into it).
{
    try { ... "boom" if True; }
    is $!.message, 'boom', '... "msg" if $x uses the given message';
}

# List prefix precedence: tighter than `or`, so `??? $x or B` is `(??? $x) or
# B`, not `??? ($x or B)`. `???` warns and its own value is falsy (matching
# `warn`'s Nil resume), so the `or`'s right side always runs.
{
    my @log;
    my $x = 0;
    ??? $x or push @log, 'or-ran';
    push @log, 'after';
    is @log.join(','), 'or-ran,after',
        '??? $x or B runs B as (??? $x) or B, then continues';
}

# Same precedence question for `...`: since `...` always dies (fail-and-throw
# at the top level), the `or`'s right side is unreachable — the *die* must
# come from evaluating `$x` alone as the message, not from `$x or say(...)`
# swallowing the whole tail as one bigger message expression.
{
    my $x = 0;
    try { ... $x or say 'unreachable'; }
    is $!.message, '0', '... $x or B dies on ($x), not on ($x or B)';
}

# Bare form (no argument, statement ends right after) keeps working.
{
    try { ...; }
    is $!.message, 'Stub code executed', 'bare ... still uses the default message';
}
