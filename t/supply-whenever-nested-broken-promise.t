use Test;

plan 2;

# `whenever <Promise> { ... }` NESTED inside a `supply { }` block's on-demand
# body (rather than directly in a `react { }` block) used to silently drop a
# broken promise instead of dying the outer react. The nested whenever's
# subscription marker is registered while `run_on_demand_body` is driving the
# body synchronously, and is turned into a `ReactSubscription` by
# `value_to_react_subscription` / `register_nested_on_demand_source` — neither
# of which had a `ValueView::Promise` arm (only `build_react_subscriptions`,
# used for a *top-level* `whenever <Promise>`, did). So the marker matched
# none of the fallback branches and was dropped on the floor.
#
# (Found via Cro::Connector.establish's `supply { whenever self.connect(...)
# -> $t { whenever $t.transformer(...) {...} } }` shape: `tcp.rakutest`
# expects connecting to a dead port to die the react instead of hanging.)

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
            flunk 'the outer whenever must not receive a value';
        }
    }
    CATCH {
        default { $died = $_; }
    }
}
ok $died.defined, 'a broken promise nested inside a supply body dies the outer react';
like $died.message, /'connect failed'/, 'the caught exception carries the broken promise\'s message';
