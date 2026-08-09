use Test;

plan 2;

# A `whenever <Promise>` directly inside a top-level `react { }` correctly
# dies the react block when the promise is broken (t/react-whenever-broken-
# promise.t, fixed by #6112). The same bug existed one level deeper: a
# `whenever <Promise>` nested INSIDE a `supply { }` block's body did not quit
# that supply when the promise broke, so a `react` consuming the supply never
# saw the death — the supply silently produced nothing and the react block
# completed normally instead of dying.
#
# Found via the vendored Cro::Core test suite (tcp.rakutest):
# `Cro::TCP::Connector.establish` is implemented as
#   supply {
#       my Promise $connection = self.connect(|%options);
#       whenever $connection -> Cro::Transform $transform { ... }
#   }
# and `establish(...).establish dies before service is started` expects the
# enclosing `react` to die when the connect Promise breaks.
{
    my $s = supply {
        my Promise $connection = Promise.new;
        $connection.break('connect failed');
        whenever $connection -> $transform {
            flunk 'the nested whenever body must not run for a broken promise';
            emit 'never';
        }
    }
    my $died;
    try {
        react {
            whenever $s -> $msg {
                flunk 'the outer whenever body must not run: the supply never emits';
            }
        }
        CATCH {
            default { $died = $_; }
        }
    }
    ok $died.defined,
        'a broken promise nested in a supply{} whenever dies the enclosing react block';
    like $died.message, /'connect failed'/,
        'the caught exception carries the broken promise\'s message';
}
