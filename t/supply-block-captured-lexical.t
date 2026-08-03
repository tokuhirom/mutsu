use Test;

plan 3;

# A `supply { … }` block's `whenever` callbacks run long after the routine that
# built the block returned, dispatched from whatever frame happens to be
# emitting. The lexicals the block captured must still resolve to what it
# captured — even when the dispatching frame has a binding of the same name,
# which is guaranteed the moment one parse site has two live instances.

{
    sub xform($tag, Supply $in --> Supply) {
        supply whenever $in -> $v { emit $tag ~ '(' ~ $v ~ ')' }
    }
    my $src = Supplier.new;
    my $s = xform('a', $src.Supply);
    $s = xform('b', $s);
    my @got;
    $s.tap(-> $v { @got.push($v) });
    $src.emit('x');
    $src.emit('y');
    is @got, ['b(a(x))', 'b(a(y))'], 'each instance keeps its own captured lexical';
}

{
    # A capture the enclosing frame reassigns AFTER building the block must
    # still be read live — it is not frozen by the ownership above.
    my $gate = 0;
    my $sup = supply { whenever Supply.from-list(1) { emit $gate } };
    $gate = 9;
    my @got;
    $sup.tap({ @got.push($_) });
    is @got, [9], 'a capture the caller reassigns later is read at tap time';
}

{
    # A capture the supply body itself writes must reach the declaring frame.
    my $seen = 0;
    my $src = Supplier.new;
    my $sup = supply { whenever $src.Supply -> $v { $seen = $v; emit $v } };
    $sup.tap(-> $v { });
    $src.emit(7);
    is $seen, 7, 'a capture the body writes is visible to the declaring frame';
}
