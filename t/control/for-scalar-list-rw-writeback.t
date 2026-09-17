use v6;
use Test;

plan 8;

# A scalar's explicit `.list` call produces one element backed by the scalar's
# own container. An aliasing loop parameter must retain that container rather
# than treating the List result as a detached value.
{
    my $x = 'raw';
    for $x.list -> $value is rw { $value = 'new' }
    is $x, 'new', 'an explicit `.list` rw parameter writes back to a Str scalar';
}

{
    my $x = 42;
    for $x.list -> $value is rw { $value += 1 }
    is $x, 43, 'the same alias works for a numeric scalar';
}

{
    my $x = 'raw';
    my $set;
    for $x.list -> $value is rw {
        $set = -> { $value = 'later' };
    }
    $set();
    is $x, 'later', 'the `.list` rw alias survives after the loop body returns';
}

{
    sub decode-in-place($values is copy) {
        for $values.list -> $value is rw {
            $value = "DECODED:$value";
        }
        $values;
    }
    is decode-in-place('raw'), 'DECODED:raw', 'a copied scalar parameter keeps `.list` writeback';
}

{
    my $x = 'raw';
    for $x.list { $_ = 'new' }
    is $x, 'new', 'the implicit topic also aliases a scalar `.list` element';
}

{
    my $x = 'raw';
    for $x.list -> \value { value = 'new' }
    is $x, 'new', 'a sigilless `.list` loop parameter aliases the scalar';
}

{
    my $x = [1, 2];
    for $x.list -> $value is rw { $value += 1 }
    is-deeply $x, [2, 3], 'an Array held by a scalar keeps its element aliases';
}

{
    my Int $x = 1;
    dies-ok { for $x.list -> $value is rw { $value = 'bad' } },
        'the scalar container constraint still applies through `.list`';
}
