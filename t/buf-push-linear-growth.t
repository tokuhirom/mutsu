use Test;

plan 23;

# `$buf.push(...)` used to round-trip the WHOLE buffer through boxed `Value`s on
# every call -- decode every existing byte to a `Value::Int`, extend, re-encode --
# so the cost of one push scaled with the buffer's current length and filling a
# buffer was O(n^2) (#7680). Only the new elements are encoded now, appended onto
# the storage node's bytes in place.
#
# This file pins both halves: that growth is linear (the 20_000-push loop below
# is minutes of work under the quadratic path and a fraction of a second under
# the byte-level one, so a regression shows up as a timeout, not a flaky
# threshold), and every semantic the element-level round trip used to provide.

# ---------------------------------------------------------------------------
# Linear growth.
# ---------------------------------------------------------------------------
{
    my $b = Buf.new;
    $b.push($_ +& 0xFF) for ^20000;
    is $b.elems, 20000, 'pushing 20_000 elements one at a time completes';
    is $b[0], 0, 'first pushed element survives';
    is $b[255], 255, 'a mid-buffer element survives';
    is $b[19999], 19999 +& 0xFF, 'last pushed element survives';
}

# ---------------------------------------------------------------------------
# Ordering, at both ends, on a buffer that already has content.
# ---------------------------------------------------------------------------
{
    my $b = Buf.new(1, 2);
    $b.push(3);
    $b.append(4, 5);
    is $b.raku, 'Buf.new(1,2,3,4,5)', 'push then append go on the end, in order';
    $b.unshift(0);
    $b.prepend(254, 255);
    is $b.raku, 'Buf.new(254,255,0,1,2,3,4,5)', 'unshift/prepend go on the front, in order';
}

# Pushing onto a buffer that has no elements yet still creates storage.
{
    my $b = Buf.new;
    $b.push(7);
    is $b.raku, 'Buf.new(7)', 'push onto an empty Buf';
    my $u = Buf.new;
    $u.unshift(7);
    is $u.raku, 'Buf.new(7)', 'unshift onto an empty Buf';
}

# ---------------------------------------------------------------------------
# The buffer is one mutable object: an alias sees the push.
# ---------------------------------------------------------------------------
{
    my $a = Buf.new(1, 2);
    my $alias = $a;
    $a.push(3);
    is $alias.raku, 'Buf.new(1,2,3)', 'a second name for the same Buf sees the push';
}

# ...but storage shared by a re-tagging coercion (.Blob/.Buf) must be forked, so
# the push is NOT seen through the other tag.
{
    my $b = Buf.new(1, 2);
    my $tag = $b.Blob;
    $b.push(9);
    is $b.elems, 3, 'the pushed-to Buf grew';
    is $tag.elems, 2, '.Blob storage shared with it did not';
    is $tag.raku, 'Blob.new(1,2)', 'and still holds its own bytes';
}

# ---------------------------------------------------------------------------
# Element width: the new elements are encoded at the width the node already
# has, not at one guessed per call.
# ---------------------------------------------------------------------------
{
    my $w = buf16.new(0x1170, 2);
    $w.push(0xABCD);
    is $w.elems, 3, 'buf16 push counts elements, not bytes';
    is $w[2], 0xABCD, 'a 16-bit element round-trips through push';
    is $w.raku, 'Buf[uint16].new(4464,2,43981)', 'buf16 keeps its element type across a push';
}

# `.Buf`/`.Blob` move the storage node under another class name without decoding
# it, so the instance's class can say width 1 while its node is width 2. The
# element round trip re-encoded at the CLASS's width and silently truncated the
# existing elements; encoding at the node's width does not.
{
    my $c = buf16.new(0x1234, 0x5678).Buf;
    is $c.list.Array, [4660, 22136], 're-tagged buf16 keeps its element values';
    $c.push(9);
    is $c.list.Array, [4660, 22136, 9],
      'pushing onto a re-tagged wide buffer does not truncate what was there';
}

# Out-of-range elements truncate to the element width, as `.new` does.
{
    my $b = Buf.new;
    $b.push(300);
    $b.push(-1);
    is $b.raku, 'Buf.new(44,255)', 'push truncates to the element width';
}

# ---------------------------------------------------------------------------
# Argument flattening: Buf/Blob, Array and Seq arguments spread their elements.
# ---------------------------------------------------------------------------
{
    my $b = Buf.new(1);
    $b.push(Buf.new(2, 3));
    $b.append([4, 5]);
    $b.append((6, 7).Seq);
    is $b.raku, 'Buf.new(1,2,3,4,5,6,7)', 'Buf/Array/Seq arguments flatten';
}

# ---------------------------------------------------------------------------
# The two checks the element-level path implemented, which the byte-level one
# must keep: Blob is immutable, and a Str element is a type error.
# ---------------------------------------------------------------------------
{
    my $blob = Blob.new(1, 2);
    dies-ok { $blob.push(3) }, 'Blob.push dies (immutable)';
    is $blob.elems, 2, 'and the Blob is unchanged';
}
{
    my $b = Buf.new(1);
    throws-like { $b.push("x") }, X::TypeCheck, 'pushing a Str is a type check failure';
    is $b.elems, 1, 'and the Buf is unchanged';
}
