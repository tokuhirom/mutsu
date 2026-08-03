use Test;

# The byte-addressed Buf accessors (`read-int*`/`read-uint*`/`read-num*` and
# their `write-*` counterparts) index a buffer's *real* storage, and their
# offset counts elements, not bytes: on a `buf32` offset 1 is element 1, i.e.
# byte 4. mutsu used to hand those methods a one-byte-per-element projection of
# the buffer, so a write both read the wrong bytes and flattened every existing
# element down to its low byte -- which is what made `Digest::MD5` produce a
# wrong digest.

plan 26;

# --- writes land on the right element ------------------------------------

{
    my $b = buf32.new(0x80636261, 0x11223344, 0x55667788);
    $b.write-uint32: 1, 0xAABBCCDD, LittleEndian;
    is $b.list.join(','), (0x80636261, 0xAABBCCDD, 0x55667788).join(','),
        'write-uint32 at offset 1 replaces element 1 of a buf32';
    is $b.elems, 3, 'an in-range write does not resize the buffer';
}

{
    my $b = buf32.new(0x80636261, 0x11223344, 0x55667788);
    $b.write-uint8: 1, 0xEE;
    is $b.list.join(','), (0x80636261, 0x112233EE, 0x55667788).join(','),
        'write-uint8 at offset 1 touches only element 1 low byte';
}

{
    my $b = buf16.new(0x1234, 0x5678, 0x9abc);
    $b.write-uint32: 1, 0xAABBCCDD, LittleEndian;
    is $b.list.join(','), (0x1234, 0xCCDD, 0xAABB).join(','),
        'a 4-byte write spans two buf16 elements';
}

{
    my $b = buf8.new(1, 2, 3, 4, 5, 6, 7, 8);
    $b.write-uint32: 1, 0xAABBCCDD, LittleEndian;
    is $b.list.join(','), (1, 0xDD, 0xCC, 0xBB, 0xAA, 6, 7, 8).join(','),
        'a width-1 buffer keeps its plain byte offset';
}

{
    my $b = buf32.new(0x80636261, 0x11223344, 0x55667788);
    $b.write-uint32: 1, 0xAABBCCDD, BigEndian;
    is $b.list.join(','), (0x80636261, 0xDDCCBBAA, 0x55667788).join(','),
        'BigEndian writes the element the other way round';
}

# --- growth follows MoarVM: `offset + size` *elements* --------------------

{
    my $b = buf32.new(1, 2);
    $b.write-uint32: 2, 0xAABBCCDD, LittleEndian;
    is $b.elems, 6, 'a write past the end grows to offset + size elements';
    is $b.list.join(','), (1, 2, 0xAABBCCDD, 0, 0, 0).join(','),
        '... with the value at the requested element and the rest zeroed';
}

{
    my $b = buf32.new(1, 2);
    $b.write-uint64: 0, 0x1122334455667788, LittleEndian;
    is $b.elems, 2, 'a write that fits does not grow the buffer';
    is $b.list.join(','), (0x55667788, 0x11223344).join(','),
        '... and spans two buf32 elements little-endian';
}

{
    my $b = buf16.new(1, 2);
    $b.write-uint64: 1, 0x1122334455667788, LittleEndian;
    is $b.elems, 9, 'buf16 grows to offset + size elements too';
    is $b.list.join(','), (1, 0x7788, 0x5566, 0x3344, 0x1122, 0, 0, 0, 0).join(','),
        '... with the eight bytes laid across four elements';
}

# --- the type-object form builds the same buffer --------------------------

{
    my $b = buf32.write-uint32(1, 0xAABBCCDD, LittleEndian);
    is $b.elems, 5, 'buf32.write-uint32 on the type object grows the same way';
    is $b.list.join(','), (0, 0xAABBCCDD, 0, 0, 0).join(','),
        '... and puts the value in element 1';
}

# --- write-num ------------------------------------------------------------

{
    my $b = buf32.new(0, 0, 0, 0);
    $b.write-num32: 1, 1.5e0, LittleEndian;
    is $b.list.join(','), (0, 0x3FC00000, 0, 0).join(','),
        'write-num32 encodes into a single buf32 element';
}

# --- reads use the same addressing ---------------------------------------

{
    my $b = buf32.new(0x80636261, 0x11223344, 0x55667788);
    is $b.read-uint32(0, LittleEndian), 0x80636261, 'read-uint32 at 0';
    is $b.read-uint32(2, LittleEndian), 0x55667788, 'read-uint32 at the last element';
    is $b.read-uint64(0, LittleEndian), 0x1122334480636261,
        'read-uint64 spans two buf32 elements';
    is $b.read-uint16(1, LittleEndian), 0x3344, 'read-uint16 reads element 1 low half';
    is $b.read-uint8(1), 0x44, 'read-uint8 reads element 1 low byte';
    is $b.read-int8(1), 0x44, 'read-int8 likewise';
}

{
    my $b = buf16.new(0x1234, 0x5678, 0x9abc, 0xdef0);
    is $b.read-uint32(1, LittleEndian), 0x9ABC5678, 'read-uint32 spans two buf16 elements';
    is $b.read-uint8(3), 0xF0, 'read-uint8 reads the fourth element low byte';
}

{
    my $b = buf8.new(1, 2, 3, 4, 5, 6, 7, 8);
    is $b.read-uint32(1, LittleEndian), 0x05040302,
        'a width-1 buffer still reads at a plain byte offset';
}

# --- out of range ---------------------------------------------------------

{
    my $b = buf32.new(1, 2, 3);
    dies-ok { $b.read-uint32(3) }, 'reading past the last element dies';
    lives-ok { $b.read-uint64(0) }, 'a read that fits within the storage lives';
}
