use Test;

# Every CONTROL handler runs INLINE at a warning's raise site, as rakudo runs
# it on top of the stack -- not only one that contains a `.resume`. An
# op-raised warning (`"a" ~ Any`) in a callee used to unwind to the region
# with its resume value in `return_value`, which the call boundary read as a
# `return`: the handler never ran and the warning was lost (#9510).

plan 16;

my @log;

sub inner() { my $x; my $y = "a" ~ $x; @log.push: 'in-after'; 3 }

sub handled() {
    { CONTROL { when CX::Warn { @log.push: 'saw' } }; inner(); @log.push: 'not reached' }
    @log.push: 'after';
    1
}
@log = ();
is handled(), 1, 'a non-resuming handler ends its region';
is-deeply @log, ['saw', 'after'], '...after seeing the callee\'s op-raised warning';

sub handled-default() {
    { CONTROL { default { @log.push: 'default' } }; inner(); @log.push: 'not reached' }
    @log.push: 'after';
    2
}
@log = ();
is handled-default(), 2, 'a `default` arm ends its region too';
is-deeply @log, ['default', 'after'], '...having run once';

sub declined() {
    { CONTROL { when CX::Take { @log.push: 'wrong arm' } }; inner(); @log.push: 'body-after' }
    3
}
@log = ();
{
    my $w;
    CONTROL { when CX::Warn { $w = .message; .resume } }
    is declined(), 3, 'a handler that matches nothing declines';
    like $w, /uninitialized/, '...so the next outer handler sees the warning';
}
is-deeply @log, ['in-after', 'body-after'], '...and resuming it continues the callee';

sub message-seen() {
    my $m = '';
    { CONTROL { when CX::Warn { $m = .message } }; inner() }
    $m
}
@log = ();
like message-seen(), /uninitialized/, 'the handler sees the warning as its topic';

# A handler in the frame that raised the warning writes that frame's locals.
my $n = 0;
{ CONTROL { when CX::Warn { $n++ } }; my $z; my $w = "b" ~ $z; @log.push: 'not reached' }
is $n, 1, 'a same-frame op-raised warning: the handler\'s write is visible';

sub counter() {
    my $c = 0;
    { CONTROL { default { $c++ } }; my $u; my $q = "x" ~ $u }
    $c
}
is counter(), 1, '...also inside a routine';

sub collect() {
    my @seen;
    { CONTROL { when CX::Warn { @seen.push: 'w' } }; inner() }
    @seen
}
is-deeply collect(), ['w'], 'a handler writes its own frame\'s array across a call';

# Nested regions: the inner handler declines, the outer one handles.
sub nested() {
    {
        CONTROL { when CX::Warn { @log.push: 'outer' } }
        { CONTROL { when CX::Next { @log.push: 'inner' } }; inner(); @log.push: 'not reached' }
        @log.push: 'not reached either'
    }
    @log.push: 'nested-after';
    4
}
@log = ();
is nested(), 4, 'a declining inner region passes the warning to the outer one';
is-deeply @log, ['outer', 'nested-after'], '...which runs once and ends its region';

# A plain `warn` in a callee still reaches a non-resuming handler.
sub warns() { warn "plain"; @log.push: 'not reached' }
sub plain() { { CONTROL { when CX::Warn { @log.push: .message } }; warns() }; 5 }
@log = ();
is plain(), 5, 'a `warn` in a callee reaches a non-resuming handler';
is-deeply @log, ['plain'], '...exactly once';

# A loop body with a handler sees each iteration's warning.
my $k = 0;
for ^3 { CONTROL { when CX::Warn { $k++ } }; inner() }
is $k, 3, 'a handler in a loop body runs for every iteration';
