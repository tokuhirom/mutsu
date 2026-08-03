use Test;

plan 6;

# `Promise($supply)` is kept when the supply is DONE, with the final value it
# emitted. A `supply { ... }` block is done once all of its `whenever`s have
# completed -- there is no explicit `done` in any of these.

{
    my $a = Promise.new;
    start { sleep 0.2; $a.keep(1) }
    my $s = supply { whenever $a -> $v { emit "got $v" } };
    is await(Promise($s)), 'got 1',
            'a supply whose only whenever completed keeps its promise';
}

{
    # Several emits: the promise is kept with the last one.
    my $a = Promise.new;
    start { sleep 0.2; $a.keep(1) }
    my $s = supply { whenever $a -> $v { emit "first"; emit "second"; emit "third" } };
    is await(Promise($s)), 'third', 'the promise is kept with the final value';
}

{
    # No emits at all: Nil, not a hang.
    my $a = Promise.new;
    start { sleep 0.2; $a.keep(1) }
    my $s = supply { whenever $a -> $v { } };
    is await(Promise($s)), Nil, 'a supply that emitted nothing keeps with Nil';
}

{
    # A `whenever` registered from inside another whenever's body must be
    # driven too -- the shape every Cro::HTTP::Client request is written in.
    my $outer = Promise.new;
    my $inner = Promise.new;
    start { sleep 0.2; $outer.keep(2); sleep 0.2; $inner.keep(9) }
    my $s = supply {
        whenever $outer -> $x {
            whenever $inner -> $y { emit "$x/$y" }
        }
    };
    is await(Promise($s)), '2/9', 'a nested whenever is driven and its emit wins';
}

{
    # Two levels of nesting.
    my ($a, $b, $c) = Promise.new xx 3;
    start { sleep 0.2; $a.keep('a'); sleep 0.2; $b.keep('b'); sleep 0.2; $c.keep('c') }
    my $s = supply {
        whenever $a -> $x {
            whenever $b -> $y {
                whenever $c -> $z { emit "$x$y$z" }
            }
        }
    };
    is await(Promise($s)), 'abc', 'whenevers nest to any depth under a Promise';
}

{
    # An explicit `done` still wins immediately, with the value emitted before it.
    my $a = Promise.new;
    start { sleep 0.2; $a.keep(1) }
    my $s = supply { whenever $a { emit 'early'; done } };
    is await(Promise($s)), 'early', 'an explicit done keeps with the last emit';
}
