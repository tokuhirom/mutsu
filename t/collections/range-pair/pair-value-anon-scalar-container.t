use Test;

# `k => my $ = v` makes the anonymous scalar's container the Pair's value, so
# the value is writable: directly, through an `is rw` loop alias over `.value`
# or `.kv`, and from a closure another routine runs. The write lands in the
# container, never in a copy a frame's return merge has to carry back
# (ADR-12529 §3, slice 27). Checked against rakudo.

plan 10;

sub run(&c) { c() }

{
    my $p = (a => my $ = 42);
    $p.value = 5;
    is $p.value, 5, 'the value of a pair over an anonymous scalar is assignable';
    is $p.value.VAR.^name, 'Scalar', 'and it is a Scalar container';
    is $p.raku, ':a(5)', 'which the pair renders as its content';
}

{
    my $p = (a => my $ = 42);
    for $p.value -> $v is rw { $v += 1 }
    is $p.value, 43, 'an is rw alias over .value writes the container';
}

{
    my $p = (a => my $ = 42);
    run { for $p.kv -> $k, $v is rw { $v += 100 } };
    is $p.value, 142, 'an is rw alias over .kv in a closure another routine runs';
}

{
    my $p = (a => my $ = 42);
    sub bump($pair) { $pair.value = 7 }
    bump($p);
    is $p.value, 7, 'a routine the pair is passed to writes it';
}

{
    my @l = (a => my $ = 5),;
    @l[0].value = 9;
    is @l[0].value, 9, 'a pair kept in a list stays writable';
}

{
    my %h = (a => my $ = 1), (b => 2);
    is %h<a>, 1, 'a hash built from such a pair takes its content';
}

is (a => my $ = Nil).value.raku, 'Any', 'Nil stored into it leaves the default';
is (x => my $ = (1, 2)).value.elems, 2, 'an itemized list stays one item inside';
