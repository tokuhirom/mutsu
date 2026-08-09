use Test;

plan 3;

# `react { whenever <Promise> { ... } }` used to treat every settled Promise
# as kept: the helper thread that waits on the promise sent Emit+Done
# unconditionally, so a *broken* promise's exception value was bound to the
# whenever's topic and the body ran as if the promise had succeeded, instead
# of dying. (Found via Cro::Core's tcp.rakutest: connecting to a TCP port
# with nothing listening breaks the connect Promise, and the react block was
# expected to die rather than treat the exception as a connection.)

{
    my $p = Promise.new;
    $p.break('boom');
    my $died;
    try {
        react {
            whenever $p {
                flunk 'the whenever body must not run for a broken promise';
                done;
            }
        }
        CATCH {
            default { $died = $_; }
        }
    }
    ok $died.defined, 'a broken promise source dies the react block instead of running the body';
    like $died.message, /boom/, 'the caught exception carries the broken promise\'s message';
}

{
    # A kept promise must still run the whenever body as before.
    my $p = Promise.new;
    $p.keep(42);
    my $got;
    react {
        whenever $p -> $v {
            $got = $v;
            done;
        }
    }
    is $got, 42, 'a kept promise source still runs the whenever body with its value';
}
