use Test;

plan 5;

# A `supply { ... }` block is a scope of its own: a `my` variable declared
# inside it is private to the block. It must neither be replaced by a
# same-named lexical of the calling scope (the `whenever` bodies run from the
# emitting thread, whose ambient env is the caller's), nor escape into that
# caller when the block returns.

# --- The block's own `my` does not escape to the caller ------------------
{
    sub mk() { supply { my $buffer = "INNER"; $buffer = $buffer ~ "+"; emit $buffer } }
    my $buffer = "OUTER";
    my @got;
    mk().tap(-> $v { @got.push($v) });
    is @got.join(','), 'INNER+', 'the supply body sees its own lexical';
    is $buffer, 'OUTER', "the block's `my` does not leak into the caller";
}

# --- A `whenever` body reads the block's lexical, not the caller's --------
{
    sub mk($in) {
        supply {
            my $buffer = Buf.new;
            whenever $in -> $packet {
                emit $buffer.elems;
            }
        }
    }

    my $buffer = Buf.new(1, 2, 3);      # same name, caller scope
    my $s = Supplier.new;
    my $done = Promise.new;
    my @got;
    mk($s.Supply).tap(-> $v { @got.push($v) }, done => { $done.keep });
    start { $s.emit(Buf.new(9)); $s.done }
    await Promise.anyof($done, Promise.in(10));
    is @got.join(','), '0', 'a whenever body reads the supply block lexical';
    is $buffer.elems, 3, "the caller's same-named Buf is untouched";
}

# --- ... and still accumulates across invocations -------------------------
{
    sub mk($in) {
        supply {
            my $acc = "";
            whenever $in -> $x {
                $acc ~= $x;
                emit $acc;
            }
        }
    }

    my $s = Supplier.new;
    my @got;
    mk($s.Supply).tap(-> $v { @got.push($v) });
    $s.emit("a"); $s.emit("b"); $s.emit("c"); $s.done;
    is @got.join('|'), 'a|ab|abc',
        'the block lexical still accumulates across whenever invocations';
}
