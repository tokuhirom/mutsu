use Test;

# GH-7720: an `if`/`unless` branch is a Raku block and owns the
# `let`/`temp` saves written in its body. The statement lowering used to
# compile the branch directly, so a save survived until an unrelated outer
# scope (or the end of the program) instead of resolving at branch exit.
#
# The value-position checks use a decontainerized tail where the value itself
# is observable before the branch restores its save. The branch must still
# restore the outer variable before the following statement runs.

plan 13;

{
    my $g = 1;
    if 1 { temp $g = 9; }
    is $g, 1, '`if` restores a `temp` in a constant-selected branch';
}

{
    my $g = 1;
    my $take = True;
    if $take { temp $g = 9; }
    is $g, 1, '`if` restores a `temp` in a runtime-selected branch';
}

{
    my $g = 1;
    unless 0 { temp $g = 9; }
    is $g, 1, '`unless` restores a `temp`';
}

{
    my $g = 1;
    with 1 { temp $g = 9; }
    is $g, 1, '`with` restores a `temp` in its branch';
}

{
    my $g = 1;
    if False { temp $g = 9; } else { temp $g = 8; }
    is $g, 1, 'the selected `else` branch restores a `temp`';
}

{
    my $g = 1;
    if True { let $g = 9; Nil }
    is $g, 1, 'an undefined `if` branch value rolls back a `let`';
}

{
    my $g = 1;
    if True { let $g = 9 }
    is $g, 9, 'a defined `if` branch value commits a `let`';
}

{
    my $g = 1;
    if True { my $local = 2; temp $g = 9; }
    is $g, 1, 'a branch-local declaration does not bypass `temp` restoration';
}

{
    my $g = 1;
    if True { if True { temp $g = 9; } }
    is $g, 1, 'a nested `if` branch restores its `temp`';
}

{
    my $g = 1;
    my $value = do if True { temp $g = 9; $g + 0 };
    is $value, 9, 'a value-position `if` exposes its branch value';
    is $g, 1, 'a value-position `if` restores its `temp`';
}

{
    my $g = 1;
    sub branch-value() { if True { temp $g = 9; $g + 0 } else { 0 } }
    is branch-value(), 9, 'a routine-tail `if` exposes its branch value';
    is $g, 1, 'a routine-tail `if` restores its `temp`';
}
