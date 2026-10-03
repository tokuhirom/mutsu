use Test;

# `die` in expression position compiles to a call rather than the `Die`
# opcode. Its exception used to reach a CATCH handler with no Backtrace
# attached, because the handler ran inline at the throw site before the
# dispatch loop's generic attach (found working on #11497).

plan 2;

sub thrower {
    my $x = die("x");
}

try {
    thrower();
    CATCH {
        default {
            ok .backtrace.list.elems > 1, 'the handler sees the frames of the throw';
            ok .backtrace.list.first({ .subname eq 'thrower' }), '... including the throwing routine';
        }
    }
}
