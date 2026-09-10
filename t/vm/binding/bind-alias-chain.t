use v6;
use Test;

# `:=` binds a name to a container: every name in a bind group observes writes
# through any of them, and binding to a readonly source makes the target readonly.

plan 16;

# --- A chain of binds is one group: writing ANY member updates them all. ---
# The bind records each alias's target as the *root* of the chain, so a write to
# a leaf used to reach the root but leave the sibling aliases stale.
{
    my $x = 1;
    my $y := $x;
    my $z := $y;
    $z = 9;
    is "$x $y $z", "9 9 9", 'writing the leaf of a bind chain updates the whole group';
}
{
    my $x = 1;
    my $y := $x;
    my $z := $y;
    $y = 9;
    is "$x $y $z", "9 9 9", 'writing the middle of a bind chain updates the whole group';
}
{
    my $x = 1;
    my $y := $x;
    my $z := $y;
    $x = 9;
    is "$x $y $z", "9 9 9", 'writing the root of a bind chain updates the whole group';
}

# A four-deep chain, written from each position.
{
    my $a = 1;
    my $b := $a;
    my $c := $b;
    my $d := $c;
    $c = 7;
    is "$a $b $c $d", "7 7 7 7", 'writing an inner name of a 4-deep chain updates all';
}
{
    my $a = 1;
    my $b := $a;
    my $c := $b;
    my $d := $c;
    $d = 7;
    is "$a $b $c $d", "7 7 7 7", 'writing the last name of a 4-deep chain updates all';
}

# The two-name case (which always worked) must keep working.
{
    my $x = 1;
    my $y := $x;
    $y = 9;
    is "$x $y", "9 9", 'a two-name bind still propagates';
}

# An unrelated variable must NOT join the group.
{
    my $p = 1;
    my $q = 2;
    my $r := $p;
    $r = 5;
    is "$p $q $r", "5 2 5", 'an unbound variable is untouched by a group write';
}

# --- Binding to a readonly source makes the target readonly. ---
{
    my $died = False;
    sub ro-bind($ro) {
        my $c := $ro;
        $c = 3;
    }
    { CATCH { default { $died = True } }; ro-bind(7) }
    ok $died, 'assigning through a bind to a readonly param dies';
}
{
    my $msg = '';
    sub ro-bind2($ro) {
        my $c := $ro;
        $c = 3;
    }
    { CATCH { default { $msg = .message } }; ro-bind2(7) }
    like $msg, /readonly/, 'and the error says the variable is readonly';
}

# A bind to a writable lexical stays writable.
{
    my $w = 1;
    my $b := $w;
    lives-ok { $b = 5 }, 'a bind to a writable lexical stays writable';
    is $w, 5, 'and the write reaches the source';
}

# Reading through a readonly bind still works -- only assignment is barred.
{
    sub ro-read($ro) {
        my $c := $ro;
        $c
    }
    is ro-read(42), 42, 'a readonly bind is still readable';
}

# --- Binding to an `is rw` parameter writes through to the caller. ---
# An `is rw` param is itself an alias of the caller's variable, so a bind onto it
# must keep the param on the alias path rather than resolving straight past it to
# the caller's name (which has no slot in the callee's frame).
{
    sub rw-bind($x is rw) {
        my $c := $x;
        $c = 3;
        "$c $x"
    }
    my $v = 1;
    my $inside = rw-bind($v);
    is $inside, "3 3", 'a bind onto an rw param updates the param inside the sub';
    is $v, 3, 'and the write reaches the caller';
}

# The same, one level deeper: a chain built on top of an rw param.
{
    sub rw-bind2($x is rw) {
        my $c := $x;
        my $d := $c;
        $d = 8;
    }
    my $v = 1;
    rw-bind2($v);
    is $v, 8, 'a bind chain rooted at an rw param reaches the caller';
}

# A plain (non-rw) param is readonly, so the caller is NOT modified.
{
    sub ro-param($x) {
        my $c := $x;
        try { $c = 3 };
    }
    my $v = 1;
    ro-param($v);
    is $v, 1, 'a bind onto a readonly param cannot modify the caller';
}
