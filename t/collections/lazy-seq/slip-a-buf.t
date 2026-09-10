use Test;

# A `Buf`/`Blob` is Positional, so `|$buf` slips its *elements*, at the buffer's
# own element width. mutsu produced a one-item slip holding the buffer itself,
# so `map |*.polymod(256 xx 3), |$blob32` — how `Digest::RIPEMD` renders its
# final digest — handed the WhateverCode the whole Blob and digested a
# numified 0.

plan 12;

{
    my $b = blob32.new(7, 8);
    is (|$b).elems, 2, 'slipping a blob32 yields its elements';
    is (|$b).join(','), '7,8', '... at the buffer element width, not byte by byte';
    is (1, |$b, 2).join(','), '1,7,8,2', '... and they flatten into an enclosing list';
}

{
    my $b = Buf.new(7, 8, 9);
    is (|$b).join(','), '7,8,9', 'slipping a Buf yields its bytes';
    sub count(*@a) { @a.elems }
    is count(|$b), 3, '... reaching a slurpy as three arguments';
}

is (|utf8.new(65, 66)).join(','), '65,66', 'slipping a utf8 buffer yields its codes';
is (|blob8.new()).elems, 0, 'slipping an empty buffer yields nothing';

# A type object has no element storage and stays a single item.
is (|Buf).elems, 1, 'slipping the Buf type object keeps it whole';
is (|Blob).elems, 1, '... and the Blob type object too';

# The shape Digest::RIPEMD ends with.
{
    my $r = blob32.new(4144542350, 2056805856);
    is (map |*.polymod(256 xx 3), |$r).join(','), '142,178,8,247,224,93,152,122',
        'a slipped Blob feeds map one element at a time';
    is (blob8.new: map |*.polymod(256 xx 3), |$r).list.fmt('%02x', ''),
        '8eb208f7e05d987a', '... and renders the expected bytes';
}

# Slipping still works where it already did.
is (|(7, 8)).join(','), '7,8', 'slipping a plain list is unchanged';
