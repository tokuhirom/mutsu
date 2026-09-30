use Test;

# A `.then` callback on an already-resolved promise may run on the calling
# thread, but a `die` inside it only breaks the derived promise: the CATCH of
# the frame that registered the callback must not handle it at the `die`
# (it used to, and then handled it a second time when the promise was awaited).

plan 4;

my class E is Exception { method message { 'e' } }

{
    my $caught = 0;
    sub f() {
        Promise.kept(1).then(-> $ { die E.new });
        CATCH { default { $caught++ } }
    }
    f();
    is $caught, 0, 'CATCH does not see a die inside a resolved .then callback';
}

{
    my $caught = 0;
    sub g() {
        await Promise.kept(1).then(-> $ { die E.new });
        CATCH { default { $caught++ } }
    }
    g();
    is $caught, 1, 'awaiting the broken promise reaches CATCH exactly once';
}

{
    my @seen;
    sub h() {
        await Promise.broken('x').orelse(-> $ { die E.new });
        CATCH { when E { @seen.push: 'E' } }
    }
    h();
    is-deeply @seen, ['E'], '.orelse on a broken promise: CATCH runs once';
}

throws-like { await Promise.kept(1).then(-> $ { die E.new }) }, E,
    'throws-like on an awaited resolved .then sees exactly one exception';
