use Test;

# Regression: `react { whenever $s { } }` over an on-demand supply whose body
# is `whenever Supply.interval(...) { done }` used to busy-spin forever. The
# inner `done` is rewritten to `$emitter.done()` (it completes the inner supply,
# it does not raise the react-done signal), and the react event loop never
# observed that completion, so it kept polling the infinite interval source.
# See S17-supply/syntax.t ("No races/crashes around interval that emits done").

plan 4;

# 1. A single tap of an on-demand supply that completes via `done` terminates.
{
    my $s = supply { whenever Supply.interval(0.001) { done } }
    lives-ok {
        react { whenever $s { } }
    }, 'react over on-demand supply with inner interval+done terminates';
}

# 2. Re-tapping the same on-demand supply works (the second tap must not hang).
{
    my $s = supply { whenever Supply.interval(0.001) { done } }
    lives-ok {
        react { whenever $s { } }
        react { whenever $s { } }
    }, 're-tapping the same on-demand supply terminates';
}

# 3. The inner `done` stops the supply after the first emission, so the
#    consumer sees exactly one value.
{
    my @received;
    react {
        whenever (supply { whenever Supply.interval(0.001) { @received.push($_); done } }) {
        }
    }
    is @received.elems, 1, 'inner done stops the interval after one emission';
}

# 4. Many sequential taps complete (the original stress shape, scaled down).
{
    my $s = supply { whenever Supply.interval(0.001) { done } }
    lives-ok {
        for ^50 {
            react { whenever $s { } }
        }
    }, '50 sequential reacts over the same on-demand supply complete';
}
