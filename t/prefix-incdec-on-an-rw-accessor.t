use Test;

plan 7;

# `$obj.attr++` and `$obj.attr += 1` both routed a method-call lvalue through
# the `__mutsu_assign_method_lvalue` writeback, but the PREFIX forms had no
# `MethodCall` arm at all and fell through to `__mutsu_incdec_nomatch`:
# `++$obj.attr` died with "Cannot resolve caller prefix:<++>(...); the parameter
# requires mutable arguments".

class S { has $.count is rw = 0 }

{
    my $s = S.new;
    is ++$s.count, 1, 'prefix ++ yields the new value';
    is $s.count, 1, 'and the write reached the attribute';
    is "Visit " ~ ++$s.count, 'Visit 2', 'and it composes in an expression';
    is --$s.count, 1, 'prefix -- yields the new value';
    is $s.count, 1, 'and the write reached the attribute';
}

# The postfix forms keep their (different) value semantics.
{
    my $s = S.new;
    is $s.count++, 0, 'postfix ++ still yields the old value';
    is $s.count, 1, 'and still increments';
}

# NOTE: an accessor on an *element* (`++@a[1].count`) is out of scope — the
# postfix form does not support it either, so the two stay at parity.
