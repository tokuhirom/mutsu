use Test;

# A `supply { }` block's own `my` lexical is ONE binding, shared by every
# callback the block registers. Each callback used to capture the env by value
# and persist its writes against its own identity, so the variable behaved as a
# per-callback snapshot: a sibling `whenever` never saw the first one's writes,
# and a `LAST` phaser read the value the variable had when the block started.

plan 5;

# Sibling `whenever`s fold into the same accumulator.
{
    my $a = Supplier.new;
    my $b = Supplier.new;
    my @got;
    my $s = supply {
        my $acc = '';
        whenever $a -> $v { $acc ~= "a$v"; emit $acc }
        whenever $b -> $v { $acc ~= "b$v"; emit $acc }
    };
    $s.tap(-> $v { @got.push($v) });
    $a.emit(1);
    $b.emit(2);
    $a.emit(3);
    is-deeply @got.List, ('a1', 'a1b2', 'a1b2a3'),
        'sibling whenever bodies share the block lexical';
}

# A `LAST` phaser sees what the body accumulated.
{
    my $src = Supplier.new;
    my @got;
    my $s = supply {
        my $payload = '';
        whenever $src -> $chunk {
            $payload ~= $chunk;
            LAST emit $payload;
        }
    };
    $s.tap(-> $v { @got.push($v) });
    $src.emit('ab');
    $src.emit('cd');
    $src.done;
    is-deeply @got.List, ('abcd',), 'LAST phaser sees the accumulated lexical';
}

# The scalar-assignment shape (`+=`), which used to be lost where a container
# mutation (`Buf.append`) was not.
{
    my $p = Supplier.new;
    my @got;
    my $s = supply {
        my $sum = 0;
        whenever $p -> $v {
            $sum += $v;
            LAST emit $sum;
        }
    };
    $s.tap(-> $v { @got.push($v) });
    $p.emit(5);
    $p.emit(7);
    $p.done;
    is-deeply @got.List, (12,), 'a scalar += from the body reaches LAST';
}

# A sub declared in the block body reads the same binding. This is the shape of
# Cro's application/x-www-form-urlencoded parser, which decoded every request
# body as empty.
{
    my $src = Supplier.new;
    my @got;
    my $s = supply {
        my $payload = '';
        whenever $src -> $chunk {
            $payload ~= $chunk;
            LAST emit decode();
        }
        sub decode() { $payload.uc }
    };
    $s.tap(-> $v { @got.push($v) });
    $src.emit('ab');
    $src.emit('cd');
    $src.done;
    is-deeply @got.List, ('ABCD',), 'a nested sub reads the accumulated lexical';
}

# A container mutation still works (it always did) -- the cell must not break it.
{
    my $src = Supplier.new;
    my @got;
    my $s = supply {
        my $buf = Buf.new;
        whenever $src -> $blob {
            $buf.append($blob);
            LAST emit $buf.decode('ascii');
        }
    };
    $s.tap(-> $v { @got.push($v) });
    $src.emit('ab'.encode('ascii'));
    $src.emit('cd'.encode('ascii'));
    $src.done;
    is-deeply @got.List, ('abcd',), 'a container mutation still accumulates';
}
