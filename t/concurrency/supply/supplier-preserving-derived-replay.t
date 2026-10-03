use Test;

# A Supply derived from a Supplier::Preserving (`.map`/`.grep`) still replays
# the values its source buffered while nothing listened. mutsu registers the
# transform eagerly, at `.map` time, so a value emitted before that call, or
# between it and the first tap, used to be lost: the transform consumed it
# into a derived supply that nobody had tapped yet, and `.Channel` on such a
# supply never saw it at all (#8825).

plan 7;

{
    my $s = Supplier::Preserving.new;
    $s.emit("x");
    my @got;
    $s.Supply.map(* ~ "!").tap({ @got.push: $_ });
    is-deeply @got, ["x!"], 'emitted before .map, replayed to the mapped tap';
}

{
    my $s = Supplier::Preserving.new;
    my $m = $s.Supply.grep(*.chars).map(* ~ "!");
    $s.emit("x");
    my (@one, @two);
    $m.tap({ @one.push: $_ });
    $s.emit("y");
    $m.tap({ @two.push: $_ });
    $s.emit("z");
    is-deeply @one, ["x!", "y!", "z!"], 'emitted between .map and the tap, replayed once';
    is-deeply @two, ["z!"], 'a second tap sees only what follows it';
}

{
    my $s = Supplier::Preserving.new;
    $s.emit(1);
    $s.emit(2);
    $s.done;
    my @got;
    my $done = False;
    $s.Supply.map(* * 10).tap({ @got.push: $_ }, done => { $done = True });
    is-deeply @got, [10, 20], 'a finished source: values replayed exactly once';
    ok $done, 'a finished source: done reaches the mapped tap';
}

{
    my $s = Supplier::Preserving.new;
    $s.emit("x");
    my $c = $s.Supply.map(* ~ "!").Channel;
    is $c.receive, "x!", '.Channel on a mapped supply receives the backlog';
}

{
    my $supplier = Supplier::Preserving.new;
    my $p = start {
        my $c = $supplier.Supply.map(* ~ "!").Channel;
        ($c.receive, $c.receive)
    };
    $supplier.emit("a");
    $supplier.emit("b");
    is-deeply (await $p), ("a!", "b!"), 'mapped channel built in a start block, racing the emits';
}
