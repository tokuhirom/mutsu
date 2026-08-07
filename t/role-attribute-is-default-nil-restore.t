use Test;

# A role-composed attribute's `is default(...)` must restore after a Nil
# assignment, same as a directly-declared class attribute's. The default
# expression for a role attribute is deferred (it may reference the role's
# type parameter) and copied onto the consuming class as a raw expression at
# composition time, in a separate registry table from a direct attribute's
# already-evaluated default — the Nil-restore paths must consult both.

{
    role R { has $.w is default(21) is rw; }
    class Consumer does R { }
    my $obj = Consumer.new;
    is $obj.w, 21, 'role-composed attribute default is used for a fresh instance';
    $obj.w = Nil;
    is $obj.w, 21, 'role-composed attribute default is restored after Nil assignment';
    $obj.w = 5;
    is $obj.w, 5, 'role-composed attribute accepts an explicit rw assignment';
    $obj.w = Nil;
    is $obj.w, 21, 'role-composed attribute default is restored again after a later Nil';
}

# Two independent instances of the same role-consuming class each restore
# their own default correctly.
{
    role R2 { has $.v is default(99) is rw; }
    class C2 does R2 { }
    my $a = C2.new;
    my $b = C2.new;
    $a.v = 1;
    $b.v = Nil;
    is $a.v, 1, 'one instance keeps its explicit value';
    is $b.v, 99, 'the other instance restores the role default';
}

# A directly-declared class attribute's default still restores correctly
# alongside a role-composed one in the same program (regression guard for
# the shared Nil-restore code path).
{
    class Direct {
        has $.d is default(7) is rw;
    }
    my $obj = Direct.new;
    $obj.d = Nil;
    is $obj.d, 7, 'directly-declared class attribute default still restores after Nil';
}

done-testing;
