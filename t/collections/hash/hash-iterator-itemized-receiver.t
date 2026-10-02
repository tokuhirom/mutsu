use Test;

plan 21;

# A method call decontainerizes its invocant, so `.iterator` on an itemized
# Hash (a `$`-held hash, an element read out of an Array or Hash, `.item`,
# `$(...)`) iterates the hash's Pairs. mutsu keeps the holder's itemization
# on the value (a flag over the same `HashData`), and the iterator builder
# asked "does this flatten as an ELEMENT of another container", which made the
# whole Hash one opaque element: `pull-one` answered the Hash itself.

sub pulled($x) {
    my $it = $x.iterator;
    my @out;
    for ^20 {
        my $v = $it.pull-one;
        last if $v.raku eq 'IterationEnd';
        @out.push($v.raku);
    }
    @out.join(' ');
}

# --- the shapes from the issue --------------------------------------------------
{
    my $s = {a => 1};
    is $s.iterator.pull-one.^name, 'Pair', 'a `$`-held hash iterates its Pairs';
    is $s.iterator.pull-one.raku, ':a(1)', 'the Pair is :a(1), not ${:a(1)}';
}
{
    my %h = a => 1;
    is %h.iterator.pull-one.^name, 'Pair', 'a bare hash still iterates its Pairs';
    is $(%h).iterator.pull-one.^name, 'Pair', '$(%h).iterator';
    is %h.item.iterator.pull-one.^name, 'Pair', '%h.item.iterator';
}

# --- every itemized holder ------------------------------------------------------
{
    my %h = a => 1;
    is pulled(%h), ':a(1)', 'a hash bound to a plain `$` parameter';
    is pulled($(%h)), ':a(1)', '$(%h) passed through a `$` parameter';
    my %nested = k => {z => 1};
    is pulled(%nested<k>), ':z(1)', 'a Hash element read out of a Hash';
    my @aoh = {q => 1},;
    is pulled(@aoh[0]), ':q(1)', 'a Hash element read out of an Array';
    is pulled(my $e = %()), '', 'an empty itemized hash has no pairs';
    is pulled(Map.new((a => 1))), ':a(1)', 'a Map held in a `$` iterates its Pairs';
}

# --- the iterator protocol still behaves --------------------------------------
{
    my $s = {a => 1};
    my $it = $s.iterator;
    $it.pull-one;
    ok $it.pull-one =:= IterationEnd, 'the iterator ends after the one Pair';
    is {a => 1, b => 2}.item.iterator.pull-one.^name, 'Pair', 'a two-key hash iterates Pairs';
    is pulled({a => 1, b => 2}.item).split(' ').elems, 2, 'and yields both of them';
}

# --- receivers that were already right must stay right ------------------------
{
    my $arr = [1,2,3];
    is pulled($arr), '1 2 3', 'a `$`-held Array iterates its elements';
    my $l = (1,2,3);
    is pulled($l), '1 2 3', 'a `$`-held List iterates its elements';
    is pulled(1..3), '1 2 3', 'a Range iterates its elements';
    is pulled((1,2).Seq), '1 2', 'a Seq iterates its elements';
    is pulled("ab".NFC), '97 98', 'a Uni iterates its codepoints';
    is pulled(Buf.new(1,2)), '1 2', 'a Buf iterates its bytes';
    is pulled(5), '5', 'a plain scalar is a one-element iterator';
}
