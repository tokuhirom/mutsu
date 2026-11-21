use Test;

# ADR-18443 slice 2b: `batch`, `tail`, `produce`, `elems` and `words` over a
# live source (or over a per-tap recipe) are per-tap recipes too (#18443):
# an untapped stage runs nothing, every tap has its own buffer / accumulator /
# count, and closing the tap releases the source.

plan 14;

my $n = 0;
my $s = Supplier.new;
my $src = $s.Supply.map({ $n++; $_ });

{
    my @stages = $src.batch(:elems(2)), $src.tail(2), $src.produce(&[+]),
        $src.elems, $src.words;
    $s.emit(1);
    is $n, 0, 'untapped buffering stages run nothing';
}

{
    $n = 0;
    my $b = $src.batch(:elems(2));
    my (@a, @c);
    my $t1 = $b.tap({ @a.push: $_.List });
    $s.emit(1);
    my $t2 = $b.tap({ @c.push: $_.List });
    $s.emit(2);
    $s.emit(3);
    is $n, 5, 'each tap of a batch runs its own chain';
    is-deeply (@a, @c), ([(1, 2),], [(2, 3),]), 'each tap of a batch has its own buffer';
    $t1.close;
    $t2.close;
    $n = 0;
    $s.emit(9);
    is $n, 0, 'closed batch taps release the source';
}

{
    my $p = $s.Supply.produce(&[+]);
    my (@p1, @p2);
    my $u1 = $p.tap({ @p1.push: $_ });
    $s.emit(1);
    my $u2 = $p.tap({ @p2.push: $_ });
    $s.emit(2);
    $s.emit(3);
    is-deeply (@p1, @p2), ([1, 3, 6], [2, 5]), 'each produce tap has its own accumulator';
    $u1.close;
    $u2.close;
}

{
    my $e = $s.Supply.elems;
    my (@e1, @e2);
    my $v1 = $e.tap({ @e1.push: $_ });
    $s.emit('a');
    my $v2 = $e.tap({ @e2.push: $_ });
    $s.emit('b');
    is-deeply (@e1, @e2), ([1, 2], [1]), 'each elems tap counts from its own tap';
    $v1.close;
    $v2.close;
}

{
    my $w = $s.Supply.words;
    my (@w1, @w2);
    my $x1 = $w.tap({ @w1.push: $_ });
    $s.emit("foo ba");
    my $x2 = $w.tap({ @w2.push: $_ });
    $s.emit("r baz qu");
    $s.emit("ux ");
    is-deeply (@w1, @w2), (["foo", "bar", "baz", "quux"], ["r", "baz", "quux"]),
        'each words tap buffers its own partial word';
    $x1.close;
    $x2.close;
}

{
    my $s2 = Supplier.new;
    my $tl = $s2.Supply.map({ $n++; $_ }).tail(2);
    my (@t1, @t2);
    $n = 0;
    $tl.tap({ @t1.push: $_ });
    $s2.emit(1);
    $tl.tap({ @t2.push: $_ });
    $s2.emit(2);
    $s2.emit(3);
    $s2.done;
    is-deeply ($n, @t1, @t2), (5, [2, 3], [2, 3]), 'each tail tap keeps its own last values';
}

{
    my $s3 = Supplier.new;
    my $bb = $s3.Supply.map(* * 10).batch(:elems(2));
    my @got;
    $bb.tap({ @got.push: $_.List }, done => { @got.push: 'done' });
    $s3.emit($_) for 1..3;
    $s3.done;
    is-deeply @got, [(10, 20), (30,), 'done'], 'a batch recipe flushes its partial batch at done';
    is-deeply ($bb.live, $s3.Supply.tail.live, $s3.Supply.produce(&[+]).live,
        $s3.Supply.elems.live, $s3.Supply.words.live), (False xx 5),
        'the buffering stages are not live';
}

{
    my $s4 = Supplier.new;
    my (@all, @one);
    $s4.Supply.map(* + 1).tail(*).tap({ @all.push: $_ });
    $s4.Supply.map(* + 1).tail(1).tap({ @one.push: $_ });
    $s4.done;
    is-deeply (@all.elems, @one), (0, [Any]), 'tail of an empty live source';
}

{
    my $s5 = Supplier.new;
    my $m = 0;
    my @got;
    $s5.Supply.map({ $m++; $_ }).produce(&[+]).head(2).tap({ @got.push: $_ });
    $s5.emit($_) for 1..5;
    is-deeply ($m, @got), (2, [1, 3]), 'a completed head releases the produce below it';
}

{
    my $s6 = Supplier.new;
    my $m = 0;
    my @got;
    my $ready = Promise.new;
    my $fin = start {
        react {
            whenever $s6.Supply.map({ $m++; $_ }).produce(&[+]) -> $v {
                @got.push: $v;
                done if @got == 2;
            }
            $ready.keep;
        }
    }
    await $ready;
    $s6.emit($_) for 1..2;
    await Promise.anyof($fin, Promise.in(10));
    $s6.emit(3);
    is-deeply ($m, @got), (2, [1, 3]), 'a react over a produce recipe releases it when done';
}

{
    my $s7 = Supplier.new;
    my @b;
    my $ready = Promise.new;
    my $fin = start {
        react {
            whenever $s7.Supply.lines.batch(:elems(2)) -> @v {
                @b.push: @v.join(',');
                done if @b == 2;
            }
            $ready.keep;
        }
    }
    await $ready;
    $s7.emit("a\nb\nc");
    $s7.emit("\nd\n");
    await Promise.anyof($fin, Promise.in(10));
    is-deeply @b, ["a,b", "c,d"], 'a react over a batch of a lines recipe';
}
