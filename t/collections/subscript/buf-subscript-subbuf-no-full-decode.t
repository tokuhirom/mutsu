use Test;

plan 14;

# `$buf[$i]` and `$buf.subbuf(...)` used to decode the WHOLE buffer into boxed
# `Value`s on every call, so one subscript cost O(buffer) and a scan over a
# buffer was quadratic. Parsing a 200 KB Redis reply took ~1.6 s, long enough
# for the server's idle timeout to close the connection (#18643). A subscript
# now decodes one element, and `subbuf` copies the run as storage bytes.
#
# The loops below are minutes of work under the old path and milliseconds
# under the new one, so a regression shows up as a timeout, not as a flaky
# threshold.

{
    my $b = Buf.new((^200_000).map(* +& 0xFF));
    my int $sum = 0;
    my int $i = 0;
    while $i < 200_000 { $sum = $sum + $b[$i]; ++$i }
    is $sum, (^200_000).map(* +& 0xFF).sum, 'a subscript scan over 200_000 bytes completes';
}

{
    my $b = Blob.new((^500_000).map(* +& 0xFF));
    my $last;
    $last = $b.subbuf($_ * 10, 10) for ^2_000;
    is $last.raku, 'Blob.new(' ~ (19_990..19_999).map(* +& 0xFF).join(',') ~ ')',
        '2_000 small subbufs of a 500_000-byte Blob complete';
}

# The semantics the per-element path provided.
{
    my $b = Buf.new(1, 2, 3, 4, 5);
    is $b.subbuf(1, 3).raku, 'Buf.new(2,3,4)', 'subbuf(from, len)';
    is $b.subbuf(3).raku, 'Buf.new(4,5)', 'subbuf(from)';
    is $b.subbuf(1..2).raku, 'Buf.new(2,3)', 'subbuf(range)';
    is $b.subbuf(2, *).raku, 'Buf.new(3,4,5)', 'subbuf(from, *)';
    is $b.subbuf(5).raku, 'Buf.new()', 'subbuf at the end is empty';
    my $s = $b.subbuf(0, 2);
    $s[0] = 9;
    is $b[0], 1, 'a subbuf is a copy, not a view';
}

{
    my $w = buf16.new(0x1234, 0xABCD, 7);
    is $w.subbuf(1, 2).raku, 'Buf[uint16].new(43981,7)', 'a buf16 subbuf keeps its element width';
    is $w[1], 0xABCD, 'a buf16 subscript reads the whole element';
    my $i = Blob[int8].new(-1, -2);
    is $i[1], -2, 'a signed element subscript sign-extends';
    is $i.subbuf(1).raku, 'Blob[int8].new(-2)', 'a signed subbuf keeps its elements';
}

{
    my $c = buf16.new(0x1234, 0x5678).Buf;
    is $c.subbuf(1).list.Array, [22136], 'a re-tagged wide buffer subbufs its elements';
}

{
    is Buf.new(1, 2)[5], 0, 'a Buf reads 0 past its end';
}
