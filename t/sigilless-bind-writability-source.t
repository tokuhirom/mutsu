use Test;

# A sigilless declaration's writability comes from what its BIND SOURCE
# denotes, and from nothing else.
#
# It used to be read back off the destination local slot after the store, which
# is ambiguous: same-named `my` lexicals in one compiled unit share ONE slot, so
# when any of them is written by a directly-nested named sub the VM boxes EVERY
# declaration through that slot into a shared `ContainerRef` cell at its
# declaration site. A later `my \x := $s.uc` in a sibling block then read back a
# container and stayed writable, so an assignment raku refuses succeeded
# silently. The verdict is now taken with the bind source still on the stack.

plan 16;

# --- 1. a value bind stays immutable next to a same-named captured alias -----
{
    my $t = "a";
    my \x := $t;
    sub writer { x = 42 }
    writer();
    is $t, 42, 'the captured alias itself still writes through';
}
{
    my $s = "b";
    my \x := $s.uc;
    dies-ok { x = 9 }, 'a sibling block binding a method RESULT stays immutable';
}
{
    my $s = "c";
    my \x := $s.uc;
    my $err;
    { x = 9; CATCH { default { $err = .message } } }
    like $err, /'Cannot modify an immutable Str'/, 'and it names the immutable value';
}
{
    my \x := 5;
    dies-ok { x = 9 }, 'so does a sibling block binding a literal';
}

# The poisoning declaration is hoisted, so it reaches a block that comes
# BEFORE it textually too.
{
    my $s = "d";
    my \y := $s.uc;
    dies-ok { y = 9 }, 'a value bind that PRECEDES the captured one is immutable too';
}
{
    my $t = "e";
    my \y := $t;
    sub later-writer { y = 7 }
    later-writer();
    is $t, 7, 'and the later capturing block is unaffected';
}

# --- 2. every container-denoting source is still writable -------------------
{
    my $t = "f";
    my \c := $t;
    sub c-writer { c = 1 }
    c-writer();
    my @a = 1, 2;
    my \c2 := @a[0];
    c2 = 9;
    is-deeply @a, [9, 2], 'an array element source is writable';
}
{
    my %h = a => 1;
    my \c3 := %h<a>;
    c3 = 9;
    is %h<a>, 9, 'a hash element source is writable';
}
{
    my @a = 1, 2;
    my \c4 := @a[1 + 0];
    c4 = 9;
    is-deeply @a, [1, 9], 'a computed index is writable';
}
{
    class RwHolder { has $.v is rw }
    my $o = RwHolder.new(v => 1);
    my \c5 := $o.v;
    c5 = 9;
    is $o.v, 9, 'an `is rw` accessor result is writable';
}
{
    my @a = 1, 2;
    my \c6 := @a;
    c6.push(3);
    is-deeply @a, [1, 2, 3], 'a whole array source is writable';
}
{
    my $s = "g";
    my \c7 := $s;
    c7 = 42;
    is $s, 42, 'a plain scalar source is writable';
}

# --- 3. the list-destructuring spelling uses the same verdict ---------------
{
    my $p = 1;
    my ($q, $r) = 2, 3;
    my (\d1, \d2) := ($p, $q);
    d1 = 10;
    is $p, 10, 'a destructuring bind to a variable aliases it';
    dies-ok { my (\d3) := (5,); d3 = 9 }, 'a destructuring bind to a literal is immutable';
}

# --- 4. re-declaration in a loop settles per iteration ----------------------
{
    my @src = 1, 2;
    my $hits = 0;
    for ^2 -> $i {
        my \e := @src[$i];
        e = e + 10;
        $hits++;
    }
    is-deeply @src, [11, 12], 'a loop re-declared element alias writes through each time';
    is $hits, 2, 'both iterations ran';
}
