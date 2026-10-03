use Test;

# A value a supply emits is ONE positional argument of whatever consumes it.
# An emitted Pair used to be bound as a *named* argument instead, so a
# WhateverCode or a `-> $_` block saw no positional at all and topicalized
# whatever `$_` was lying around (#8825, MoarVM::Remote's test helper).

plan 12;

{
    my $s = Supplier.new;
    my @got;
    $s.Supply.map(*.value).tap({ @got.push: $_ });
    $s.emit(("a" => 1));
    $s.emit(("b" => 2));
    is-deeply @got, [1, 2], '.map(*.value) over emitted Pairs';
}

{
    my $s = Supplier.new;
    my @got;
    $s.Supply.grep({ .key eq "out" }).map(*.value).tap({ @got.push: $_ });
    $s.emit(("out" => "x"));
    $s.emit(("err" => "y"));
    $s.emit(("out" => "z"));
    is-deeply @got, ["x", "z"], '.grep(block).map(*.value) chain';
}

{
    my $s = Supplier.new;
    my @got;
    $s.Supply.tap(-> $_ { @got.push: .key });
    $s.emit(("k" => 7));
    is-deeply @got, ["k"], 'tap with a `-> $_` block';
}

{
    my $s = Supplier.new;
    my @got;
    $s.Supply.tap(*.value.&{ @got.push: $_ });
    $s.emit(("k" => 8));
    is-deeply @got, [8], 'tap with a WhateverCode';
}

{
    my $s = Supplier.new;
    my @got;
    $s.Supply.grep(*.value > 1).tap({ @got.push: .key });
    $s.emit(("a" => 1));
    $s.emit(("b" => 2));
    is-deeply @got, ["b"], '.grep(WhateverCode) over emitted Pairs';
}

{
    my $s = Supplier.new;
    my @got;
    $s.Supply.map(-> $p { $p.key ~ $p.value }).tap({ @got.push: $_ });
    $s.emit(("a" => 1));
    is-deeply @got, ["a1"], '.map with a named-parameter pointy block';
}

{
    my $s = Supplier.new;
    my @got;
    $s.Supply.unique(:as(*.key)).tap({ @got.push: .value });
    $s.emit(("a" => 1));
    $s.emit(("a" => 2));
    $s.emit(("b" => 3));
    is-deeply @got, [1, 3], '.unique(:as(WhateverCode)) over emitted Pairs';
}

{
    my $s = Supplier.new;
    my @got;
    $s.Supply.do(-> $_ { @got.push: .key }).tap;
    $s.emit(("d" => 1));
    is-deeply @got, ["d"], '.do with a `-> $_` block';
}

{
    my @got;
    my $s = Supplier.new;
    react {
        whenever $s.Supply.map(*.value) { @got.push: $_; done if @got == 2 }
        whenever $s.Supply -> $_ { @got.push: .key; done if @got == 2 }
        whenever Promise.in(0.05) { $s.emit(("k" => 9)) }
    }
    is-deeply @got.sort.List, (9, "k").sort.List, 'whenever over mapped and raw Pair streams';
}

{
    # A Pair given as an ordinary positional argument to the same kinds of
    # blocks is unaffected.
    my $wc = *.value;
    is $wc(("x" => 5)), 5, 'WhateverCode called directly with a Pair';
    is (-> $_ { .key })(("y" => 6)), "y", '`-> $_` called directly with a Pair';
}

{
    # The MoarVM::Remote shape: a plan loop whose `given ... -> $_ is copy`
    # rebinds `$_` while the mapped channel is read.
    my $supplier = Supplier::Preserving.new;
    my $outputs = $supplier.Supply.grep({ .key eq "stdout" }).map(*.value).Channel;
    $supplier.emit(("stdout" => "line 1"));
    $supplier.emit(("stderr" => "noise"));
    $supplier.emit(("stdout" => "line 2"));
    my @plan = (command => "a"), (command => "b");
    my @got;
    while @plan {
        given @plan.shift -> $_ is copy {
            when .key eq "command" {
                $_ = command => .value;
                @got.push: $outputs.receive;
            }
        }
    }
    is-deeply @got, ["line 1", "line 2"], 'channel read inside a `given -> $_ is copy` plan loop';
}
