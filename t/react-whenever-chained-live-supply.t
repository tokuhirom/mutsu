use Test;

plan 4;

# `react { whenever supply { whenever $live { emit ... } } { ... } }` -- a
# `supply { }` transform placed between a live source and a react. This is the
# shape every Cro pipeline stage has. Values that arrive on the *inner* source
# after the supply body has run once must still reach the outer whenever.

{
    my $s = Supplier.new;
    my $transformed = supply {
        whenever $s.Supply -> $x { emit "got-$x" }
    };
    my @seen;
    react {
        whenever $transformed -> $v {
            @seen.push($v);
            done if @seen == 2;
        }
        whenever Promise.in(0.2) { $s.emit(1); $s.emit(2) }
        whenever Promise.in(10) { done }
    }
    is-deeply @seen, ['got-1', 'got-2'],
        'values emitted after the transform body ran reach the outer whenever';
}

# `done` raised by the outer whenever body must end the react even though the
# value came through the transform (the emit path has to swallow the signal so
# the emitting body can unwind, so the loop has to notice it separately).
{
    my $s = Supplier.new;
    my $transformed = supply {
        whenever $s.Supply -> $x { emit $x * 2 }
    };
    my $after = False;
    react {
        whenever $transformed -> $v {
            done;
        }
        whenever Promise.in(0.2) { $s.emit(21) }
        whenever Promise.in(10) { $after = True; done }
    }
    nok $after, '`done` in the outer whenever body ends the react promptly';
}

# The same, but with the transform declared inside a method -- the shape
# Cro::Transform.transformer has.
{
    class Doubler {
        method transformer(Supply $in) {
            supply {
                whenever $in -> $x { emit $x * 2 }
            }
        }
    }
    my $s = Supplier.new;
    my $out = Doubler.new.transformer($s.Supply);
    my @seen;
    react {
        whenever $out -> $v {
            @seen.push($v);
            done if @seen == 3;
        }
        whenever Promise.in(0.2) { $s.emit($_) for 1..3 }
        whenever Promise.in(10) { done }
    }
    is-deeply @seen, [2, 4, 6], 'a transform declared in a method streams too';
}

# Two transforms chained -- a pipeline, not a single stage.
{
    my $s = Supplier.new;
    my $one = supply { whenever $s.Supply -> $x { emit $x + 1 } };
    my $two = supply { whenever $one -> $x { emit $x * 10 } };
    my @seen;
    react {
        whenever $two -> $v {
            @seen.push($v);
            done if @seen == 2;
        }
        whenever Promise.in(0.2) { $s.emit(1); $s.emit(2) }
        whenever Promise.in(10) { done }
    }
    is-deeply @seen, [20, 30], 'two chained transforms both stream';
}

done-testing;
