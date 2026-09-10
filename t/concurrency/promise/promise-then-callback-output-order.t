use Test;

# A `.then` callback runs on a worker thread. Its output must interleave with
# the main thread's output in the same order Rakudo produces: the callback's
# output appears while the main thread is blocked in `.result`, before the
# statement that reads `.result` proceeds. Regression pin for the drain at the
# `.result` synchronization point.

plan 4;

{
    my $p1 = Promise.new;
    my $p2 = $p1.then(-> $res {
        is $res.result, 42, "callback sees kept value";   # test 1
        101
    });
    $p1.keep(42);
    is $p2.result, 101, "then promise result";            # test 2
}

{
    my $p1 = Promise.new;
    my $p2 = $p1.then(-> $res {
        is $res.status, Broken, "callback sees broken status"; # test 3
        "recovered"
    });
    $p1.break("boom");
    is $p2.result, "recovered", "then promise recovered value"; # test 4
}
