use Test;

# #9216, the residue of #9200's `substr-rw` bound-Proxy fix:
#   1. `subbuf-rw` is a core routine, not just an assignment-only spelling;
#   2. `$b.subbuf-rw(...)` / `subbuf-rw($b, ...)` bound with `:=` is a Proxy
#      whose STORE splices into the buffer;
#   3. a `substr-rw` Proxy's window follows its STOREs (rakudo's Proxy moves its
#      char count to the stored string's length), while a `subbuf-rw` Proxy
#      keeps its original window, as rakudo's does.

plan 14;

{
    my $b = Buf.new(1, 2, 3, 4);
    my $r := subbuf-rw($b, 1, 2);
    $r = Buf.new(9);
    is-deeply $b, Buf.new(1, 9, 4), 'subbuf-rw as a routine: store through the bound Proxy';
}

{
    my $b = Buf.new(1, 2, 3, 4);
    my $r := $b.subbuf-rw(1, 2);
    is-deeply $r, Buf.new(2, 3), 'method form: the Proxy fetches the window';
    $r = Buf.new(9);
    is-deeply $b, Buf.new(1, 9, 4), 'method form: store through the bound Proxy';
    $r = Buf.new(7, 7, 7);
    is-deeply $b, Buf.new(1, 7, 7, 7), 'a subbuf-rw Proxy keeps its original window';
}

{
    my $b = Buf.new(1, 2, 3, 4);
    my $alias = $b;
    my $r := $b.subbuf-rw(0, 1);
    $r = Buf.new(8);
    is-deeply $alias, Buf.new(8, 2, 3, 4), 'the store reaches every alias of the buffer';
}

{
    my $b = Buf.new(1, 2, 3);
    subbuf-rw($b, 0, 1) = Buf.new(5);
    is-deeply $b, Buf.new(5, 2, 3), 'the assignment form of the routine still works';
    $b.subbuf-rw(2) = Buf.new(6, 6);
    is-deeply $b, Buf.new(5, 2, 6, 6), 'and so does the method assignment form';
}

{
    my $s = "pqab";
    my $r := substr-rw($s, 1, 1);
    $r = "ZZ";
    is $s, 'pZZab', 'substr-rw: first store';
    $r = "W";
    is $s, 'pWab', 'the second store replaces the whole previous replacement';
    is $r, 'W', 'and the Proxy fetches the new window';
}

{
    my $s = "pqab";
    my $r := $s.substr-rw(1, 1);
    $r = "ZZ";
    is $s, 'pZZab', 'method form: first store';
    $r = "W";
    is $s, 'pWab', 'method form: the window follows the store';
}

sub in-sub {
    my $s = "hello";
    my $r := substr-rw($s, 0, 1);
    $r = "JJ";
    $r = "Y";
    $s
}
is in-sub(), 'Yello', 'the tracked window works inside a routine too';

{
    my $s = "abc";
    my $r := substr-rw($s, 1, 1);
    $r = "";
    $r = "XY";
    is $s, 'aXYc', 'a store that empties the window leaves a zero-width insertion point';
}
