use Test;

plan 4;

# A `whenever` body is a block: a `my` variable it declares is private to that
# block and must not be written back into whatever scope invoked it. The body
# is dispatched by the thread that emits into the supply, so a leak lands in
# that thread's scope — where it silently rebinds a caller lexical of the same
# name between two emits.

{
    sub mk($in) {
        supply {
            whenever $in -> $packet {
                my $data = "INNER-" ~ $packet;
                emit $data;
            }
        }
    }

    my $data = "OUTER";
    my $s = Supplier.new;
    my $done = Promise.new;
    my @got;
    mk($s.Supply).tap(-> $v { @got.push($v) }, done => { $done.keep });
    start {
        $s.emit($data);
        $s.emit($data);
        $s.done;
    }
    await Promise.anyof($done, Promise.in(10));
    is @got.join('|'), 'INNER-OUTER|INNER-OUTER',
        "the emitter's lexical survives the whenever body's same-named `my`";
    is $data, 'OUTER', "the whenever body's `my` does not reach the caller";
}

# The body's own lexical still accumulates across invocations when it is
# declared by the enclosing supply block rather than the whenever body.
{
    sub mk($in) {
        supply {
            my $seen = 0;
            whenever $in -> $x {
                my $seen-here = $x * 10;
                $seen += $x;
                emit "$seen/$seen-here";
            }
        }
    }

    my $seen-here = 'CALLER';
    my $s = Supplier.new;
    my @got;
    mk($s.Supply).tap(-> $v { @got.push($v) });
    $s.emit(1); $s.emit(2); $s.done;
    is @got.join('|'), '1/10|3/20',
        'the supply block lexical accumulates while the body lexical is fresh';
    is $seen-here, 'CALLER', "the body's `my` did not clobber the caller's";
}
