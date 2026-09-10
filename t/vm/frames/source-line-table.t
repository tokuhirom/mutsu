use v6;
use Test;

# Pin for the ip -> line table (ADR-0006 2.3): the per-statement `SetSourceLine`
# opcode is gone; `CompiledCode` carries a static ip -> line table instead and
# the VM derives the current line from the executing instruction. Every consumer
# of the current line (callframe records, backtraces) must still see the exact
# statement line — including after an inner loop/block body has run, which the
# opcode-based scheme only got right because call sites re-emitted
# `SetSourceLine` defensively.

plan 7;

sub caller-line { callframe(1).line }

# 1. Plain call site.
is caller-line(), 17, 'call site line at the top level';

# 2. Call site inside a loop body.
my @in-loop;
for 1..2 {
    @in-loop.push(caller-line());
}
is @in-loop.join(','), '22,22', 'call site line inside a for body';

# 3. Call site AFTER an inner loop body ran in the same block: the line must be
#    the call's own, not the loop body's last line.
my @after-loop;
{
    for 1..2 { my $tmp = 1 }
    @after-loop.push(caller-line());
}
is @after-loop.join(','), '31', 'call site line after an inner loop body';

# 4. Call site in an expression that also contains a `do` block.
my $mixed = do { my $z = 0; $z } + caller-line();
is $mixed, 36, 'call site line in an expression containing a do block';

# 5. A call from inside a sub, after that sub ran a loop of its own.
sub outer {
    for 1..2 { my $tmp = 1 }
    caller-line();
}
is outer(), 42, 'call site line inside a sub body after a loop';

# 6. A sub-to-sub call site: the callee's caller frame carries the inner line.
sub middle { caller-line() }
is middle(), 47, 'call site line inside another sub';

# 7. A backtrace records the throwing sub's own body line.
sub thrower { die "boom" }
try {
    thrower();
    CATCH {
        default {
            is .backtrace.list.first({ .subname eq 'thrower' }).line, 51,
                'the throwing sub frame carries its die line';
        }
    }
}
