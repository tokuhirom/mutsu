use Test;

# ADR-0068: an aliased container's *structural* mutation must exclude a
# concurrent writer.
#
# The name-keyed cross-thread lanes (`__mutsu_atomic_arr::`) cover the case
# where the thread body mentions the container itself. They decline for a
# container that has been boxed into a shared `ContainerRef` cell, on the
# premise that the cell's Mutex already protects it -- it does not: it guards
# the cell's `Value`, and the element store releases it before mutating the
# container that `Value` points at.
#
# Every block below reaches the container through a route the thread-escape
# analysis cannot see (a named sub, or a tap callback), so the container is
# celled and the write takes the general assignment path rather than the lane.
# Before the fix these raced on one `Vec<Value>`: silently lost updates, and
# `double free or corruption` from two concurrent `Vec::resize` calls.

plan 6;

# A named sub is the aliasing edge: the `start` body mentions `&put-it`, never
# `@a`, so nothing in the block's free variables says the container escapes to
# a thread.
{
    my @a;
    sub put-it($i) { @a[$i] = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { put-it($t * 50 + $k) } } };
    is @a.grep(*.defined).elems, 1000,
        'every element store through a celled array lands';
}

# The hash twin of the same route.
{
    my %h;
    sub set-it($k) { %h{$k} = 1 }
    await (^20).map: -> $t { start { for ^50 -> $k { set-it("k{$t * 50 + $k}") } } };
    is %h.elems, 1000, 'every key store through a celled hash lands';
}

# The shape ADR-0068 §3 measured corrupting the heap: three writers into one
# celled array, driven from twenty concurrent emitters through plain `.tap`
# callbacks (`.tap` has no serialization guarantee, so the `.act` remedy does
# not apply here).
{
    my @seen;
    sub len() { @seen.elems }
    my $supplier = Supplier.new;
    my $supply = $supplier.Supply;
    $supply.tap: { @seen[$_]   = "Fizz" if $_ %% 3 }
    $supply.tap: { @seen[$_]  ~= "Buzz" if $_ %% 5 }
    $supply.tap: { @seen[$_] //= $_ }
    await do for 1..20 { start { sleep rand / 10; $supplier.emit($_) } }
    is len(), 21, 'concurrent tap writers all land in the celled array';
    is @seen[1..20].join(' '),
        "1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz",
        'and each element holds what the three writers composed';
}

# ADR-0068 §11, route 5: the same celled-container writes driven from a
# `Channel.Supply` tap rather than a `Supplier` one. The route was the last one
# §3 left "Exposed" on the path oracle; with the probe idiom corrected to a
# SINGLE tap -- rakudo makes two taps on a channel-backed Supply competing
# consumers, so a three-tap fan-out means something different there (#7604) --
# the capture shape reaches the cell-keyed guard 100/100 and the unsynchronized
# aliased store 0 times.
{
    my @seen;
    sub chan-len() { @seen.elems }
    my $chan = Channel.new;
    $chan.Supply.tap: {
        @seen[$_]   = "Fizz" if $_ %% 3;
        @seen[$_]  ~= "Buzz" if $_ %% 5;
        @seen[$_] //= $_;
    }
    await do for 1..20 { start { sleep rand / 10; $chan.send($_) } }
    $chan.close;
    is chan-len(), 21, 'three writers behind a Channel.Supply tap all land';
    is @seen[1..20].join(' '),
        "1 2 Fizz 4 Buzz Fizz 7 8 Fizz Buzz 11 Fizz 13 14 FizzBuzz 16 17 Fizz 19 Buzz",
        'and each element holds what the three writers composed';
}
