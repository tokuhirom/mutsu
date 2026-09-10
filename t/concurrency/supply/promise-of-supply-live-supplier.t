use v6;
use Test;

plan 3;

# `Promise($supply)` over a supply block whose `whenever` source is a live
# Supplier must wait for that supplier: the promise is kept when the supply
# completes, not when the source happens to be empty at subscription time.
# Treating a live supplier-backed source as a finite one resolved the promise
# (and ran the LAST phaser) before the producer had emitted anything.

{
    my $p = Supplier.new;
    my $s = $p.Supply;
    start {
        sleep 0.2;
        $p.emit(Buf.new(1, 2, 3));
        sleep 0.1;
        $p.emit(Buf.new(4, 5));
        $p.done;
    }
    my $promise = Promise(supply {
        my $joined = Buf.new;
        whenever $s -> $blob {
            $joined.append($blob);
            LAST emit $joined;
        }
    });
    is (await $promise).elems, 5,
        'the promise waits for a live Supplier source to finish';
}

{
    my $p = Supplier::Preserving.new;
    $p.emit(Buf.new(9));
    my $s = $p.Supply;
    start {
        sleep 0.2;
        $p.emit(Buf.new(8, 7));
        $p.done;
    }
    my $promise = Promise(supply {
        my $joined = Buf.new;
        whenever $s -> $blob {
            $joined.append($blob);
            LAST emit $joined;
        }
    });
    is (await $promise).list.join(","), "9,8,7",
        'a preserving source delivers its backlog and its later emits';
}

# A source that is already finished still resolves immediately.
{
    my $p = Supplier::Preserving.new;
    $p.emit(Buf.new(1, 2));
    $p.done;
    my $promise = Promise(supply {
        my $joined = Buf.new;
        whenever $p.Supply -> $blob {
            $joined.append($blob);
            LAST emit $joined;
        }
    });
    is (await $promise).elems, 2,
        'a source that finished before the subscription resolves at once';
}
