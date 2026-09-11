use Test;

# A resumed CATCH handler runs inline at the throw site and writes its changes
# back into the frame that installed it. That writeback has to be drained at
# every call site — including a subscript assignment that dispatches a user
# `ASSIGN-KEY`, which is a method call without a call opcode of its own.

plan 4;

class X::Nope is Exception { method message() { "nope" } }

class Guarded {
    has %.store;
    method AT-KEY($key)         { %!store{$key} // 0 }
    method ASSIGN-KEY($key, $v) { $v >= 0 ?? (%!store{$key} = $v) !! X::Nope.new.throw }
}

my $g = Guarded.new;
{
    my $caught = False;
    CATCH {
        $caught = True;
        when X::Nope { .resume }
        default { .resume }
    }
    $g<a> = -1;
    ok $caught, 'the CATCH flag survives an ASSIGN-KEY throw';
    is $g<a>, 0, 'and the rejected assignment left nothing behind';
}
{
    my $caught = False;
    CATCH {
        $caught = True;
        default { .resume }
    }
    $g<a> = 5;
    nok $caught, 'an accepted assignment throws nothing';
    is $g<a>, 5, 'and stores the value';
}

# vim: expandtab shiftwidth=4
