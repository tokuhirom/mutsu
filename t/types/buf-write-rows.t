use Test;

# `Buf`'s `write-*` family as rows (ADR-11276 §9.170): a type object answers a
# fresh buffer, an instance is written in place through every holder, a `Blob`
# has no such method, and a wrong argument count is Rakudo's bind error.
# `Blob`'s mutator protos refuse a `Blob:D`. Expected values are Rakudo's.

plan 28;

# Type objects answer a fresh buffer.
is-deeply buf8.write-uint16(0, 258), buf8.new(2, 1), 'buf8.write-uint16 on the type object';
is-deeply buf8.write-num32(0, 1e0), buf8.new(0, 0, 128, 63), 'buf8.write-num32';
is-deeply buf8.write-bits(0, 4, 15), buf8.new(0xF0), 'buf8.write-bits on the type object';
is-deeply buf8.write-ubits(4, 4, 15), buf8.new(0x0F), 'buf8.write-ubits on the type object';
is-deeply Buf.write-int32(1, 7, BigEndian), Buf.new(0, 0, 0, 0, 7), 'Buf.write-int32 big-endian';

# Instances are written in place, through every holder.
{
    my $b = buf8.new(0, 0, 0, 0);
    my $r = $b.write-uint8(1, 300);
    ok $r === $b, 'write-uint8 answers the receiver';
    is $b.list, (0, 44, 0, 0), '... wrapped to the element width';
    my $c = $b;
    $c.write-int16(2, -2);
    is $b.list, (0, 44, 254, 255), 'an alias sees the write';
    my $m = 'write-uint8';
    $b."$m"(0, 7);
    is $b[0], 7, 'a run-time method name';
    my \s = buf8.new(0, 0);
    s.write-uint16(0, 513);
    is s.list, (1, 2), 'a sigilless binding';
    my @a = buf8.new(0, 0);
    @a[0].write-uint8(1, 5);
    is @a[0].list, (0, 5), 'an array element';
    my %h = k => buf8.new(0);
    %h<k>.write-uint8(0, 9);
    is %h<k>.list, (9,), 'a hash value';
    is buf8.new(1, 2).write-uint8(0, 9).list, (9, 2), 'a temporary';
    sub f { my $x = buf8.new(0); { $x.write-uint8(0, 4) }; $x }
    is f().list, (4,), 'a write in a nested block reaches the binding';
}

# No such method on a Blob or an encoding buffer.
throws-like { Blob.new(1, 2).write-uint8(0, 5) }, X::Method::NotFound, 'Blob instance';
throws-like { "A".encode.write-uint8(0, 66) }, X::Method::NotFound, 'utf8 instance';
throws-like { blob8.write-uint8(0, 5) }, X::Method::NotFound, 'blob8 type object';

# A wrong argument count is the signature's bind error.
throws-like { buf8.new.write-uint8(0) }, Exception,
    message => /'Too few positionals passed; expected 3 or 4 arguments but got 2'/,
    'write-uint8 with one argument';
throws-like { buf8.new.write-bits(0, 4) }, Exception,
    message => /'Too few positionals passed; expected 4 arguments but got 3'/,
    'write-bits with two arguments';

# Blob's mutator protos have no candidate for a Blob:D; pop/shift/reallocate
# are not Blob methods at all.
throws-like { Blob.new(1).push(2) }, X::Multi::NoMatch, 'Blob.push';
throws-like { Blob.new(1).append(2) }, X::Multi::NoMatch, 'Blob.append';
throws-like { "a".encode.unshift(1) }, X::Multi::NoMatch, 'utf8.unshift';
throws-like { my $bl = Blob.new(1); $bl.pop }, X::Method::NotFound, 'Blob.pop';
throws-like { Blob.new(1).reallocate(4) }, X::Method::NotFound, 'Blob.reallocate';

# A user class composing a buffer role is the buffer it composes.
{
    class UBuf does Buf[uint8] is repr('VMArray') { }
    my $u = UBuf.new(0, 0);
    $u.write-uint16(0, 513);
    is $u.list, (1, 2), 'write-uint16 on a Buf-composing user class';
    is UBuf.write-uint8(0, 5).^name, 'UBuf', '... and on its type object';
    class UBlob does Blob[uint8] is repr('VMArray') { }
    throws-like { UBlob.new(1).push(2) }, X::Multi::NoMatch, 'a Blob-composing class refuses push';
    throws-like { UBlob.new(1).write-uint8(0, 2) }, X::Method::NotFound, '... and has no write-uint8';
}
