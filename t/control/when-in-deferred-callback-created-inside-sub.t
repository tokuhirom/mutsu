use v6;
use Test;

plan 4;

# A bare `when` inside a `.tap: { ... }` callback must consume its own
# `succeed` signal at the callback's own frame boundary, exactly like a
# top-level bare block does. The bug: when the callback closure was CREATED
# inside a `sub`/`method` (so its captured env carries `__mutsu_callable_id`
# for supporting an explicit `return` back to that routine), the succeed
# signal -- which also carries a `return_value` to pass the matched branch's
# tail value up through an enclosing `given`/`for` -- got misidentified as
# an explicit `return` targeting that routine. Since the routine that
# registered the tap had already returned by the time the supply emitted
# (the callback fires later, asynchronously), that misrouted "return" had no
# live frame to land in and escaped as an uncaught runtime error.

{
    my @log;
    sub setup(Supplier $s) {
        $s.Supply.tap: {
            when Int { @log.push("int:$_") }
            when Str { @log.push("str:$_") }
        };
    }
    my $s = Supplier.new;
    setup($s);
    $s.emit(42);
    $s.emit("hi");
    is @log.join(','), 'int:42,str:hi',
        'a when in a tap callback created inside a sub does not leak succeed';
}

{
    my @log;
    class C {
        has Supplier $.s = Supplier.new;
        method setup() {
            $!s.Supply.tap: {
                when Int { @log.push("int:$_") }
                when Str { @log.push("str:$_") }
            };
        }
    }
    my $c = C.new;
    $c.setup;
    $c.s.emit(7);
    is @log.join(','), 'int:7',
        'a when in a tap callback created inside a method does not leak succeed';
}

{
    my @log;
    class D {
        has Supplier $.s = Supplier.new;
        submethod TWEAK() {
            $!s.Supply.tap: {
                when Int { @log.push("int:$_") }
            };
        }
    }
    my $d = D.new;
    $d.s.emit(9);
    is @log.join(','), 'int:9',
        'a when in a tap callback created inside TWEAK does not leak succeed';
}

# A genuine `return` from a callback created inside a sub/method that is
# STILL live (synchronous invocation) is unaffected -- it must still
# non-locally return to that routine.
{
    sub outer() {
        my @a = (1, 2, 3, 4, 5);
        return @a.first({ return $_ * 10 if $_ == 3; False });
    }
    is outer(), 30, 'a genuine return from a live enclosing routine still works';
}
