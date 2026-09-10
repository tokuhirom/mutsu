use Test;

plan 14;

class C { has $.v is rw }
sub g($y is rw) { $y = 9 }

# A `Method` value invoked as a CODE value takes its invocant as positional
# argument 0, so the `is rw` parameter is positional 1 counting it.
{
    class M { method m($y is rw) { $y = 9 } }
    my $c = C.new(v => 1);
    my $obj = M.new;
    $obj.^lookup('m')($obj, $c.v);
    is $c.v, 9, 'a .^lookup method called as a code value binds its rw argument';

    my $l = $obj.^lookup('m');
    my $c2 = C.new(v => 1);
    $l($obj, $c2.v);
    is $c2.v, 9, 'and the same through a variable holding it';
}

# `is rw` on an OPTIONAL parameter is refused at declaration time. A named
# parameter is optional unless it carries `!`.
throws-like 'sub h(:$y is rw) { }', X::Trait::Invalid,
    'is rw on an optional NAMED parameter is refused';
throws-like 'class K { method m(:$y is rw) { } }', X::Trait::Invalid,
    'the method spelling too';
throws-like 'sub h($y? is rw) { }', X::Trait::Invalid,
    'the `?`-marked spelling still is';
lives-ok { EVAL 'sub h(:$y! is rw) { }' },
    'a REQUIRED named parameter is fine';

# An OUT-OF-RANGE subscript argument vivifies: an argument position is a
# definite bind, so the element is grown rather than declined.
{
    my @a = 1, 2;
    my $r = &g;
    $r(@a[5]);
    is @a.raku, '[1, 2, Any, Any, Any, 9]', 'a code-value callee grows the array';
}
{
    my @a = 1, 2;
    my $b = { $_ = 9 };
    $b(@a[5]);
    is @a.raku, '[1, 2, Any, Any, Any, 9]', 'a bare-block topic grows it too';
}
{
    my @a = 1, 2;
    g(@a[5]);
    is @a.raku, '[1, 2, Any, Any, Any, 9]', 'the named callee is unchanged';
}
{
    my %h;
    my $r = &g;
    $r(%h<k>);
    is-deeply %h, {k => 9}, 'a missing hash key vivifies for a code-value callee';
}
{
    my %h;
    my $b = { $_ = 9 };
    $b(%h<k>);
    is-deeply %h, {k => 9}, 'and for a bare-block topic';
}

# The in-range cases the headline fix closed are unmoved.
{
    my @a = 1, 2;
    my $r = &g;
    $r(@a[0]);
    is-deeply @a.List, (9, 2), 'an in-range element still binds';
}
{
    my @a = 1, 2;
    (-> $x is rw { $x = 9 })(@a[1]);
    is-deeply @a.List, (1, 9), 'a pointy block binds it';
}

# A receiver subscript past the end is NOT a bind and must not grow.
{
    my @a = 1, 2;
    my $ignored = @a[5];
    is @a.elems, 2, 'a plain out-of-range read leaves the array alone';
}
