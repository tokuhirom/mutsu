use Test;

plan 5;

# A `$` alias of an object-hash attribute must key its writes by `.WHICH`
# like the accessor does; a raw key landed beside the `.WHICH`-keyed entry
# and later `$obj.attr<k> = v` writes went missing.
class H { has %.u{Str:D} is rw }

{
    my $a = H.new;
    my $t := $a.u;
    $t<o> = 1;
    $a.u<o> = 7;
    is $a.u<o>, 7, 'accessor write after alias write wins';
    is $t<o>, 7, 'alias sees the accessor write';
    is $a.u.elems, 1, 'one entry, not a raw and a .WHICH-keyed pair';
}

{
    my $a = H.new;
    my $t := $a.u;
    $t<o> = 1;
    $t<o>++;
    $a.u<o> = $a.u<o> + 10;
    is $a.u<o>, 12, 'read-modify-write through both spellings';
    is $a.u.keys.elems, 1, 'still one entry';
}
