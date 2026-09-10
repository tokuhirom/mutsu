use Test;

# Coherence pin for the env<->locals single-store path
# (MUTSU_NO_BLANKET_RECONCILE): `substr-rw($s, ...) = v` (and subbuf-rw)
# mutate the target string/buf and write it back to `env[$s]`, but the
# call-site must also refresh the caller's local slot. Without the blanket
# reconcile the slot kept the original value. Must pass identically with and
# without the blanket reconcile.

plan 6;

{
    my $str = "gorch ding";
    substr-rw($str, 0, 5) = "gloop";
    is $str, "gloop ding", "substr-rw lvalue assign reaches the local slot";
}

{
    my $str = "Hello World";
    substr-rw($str, 6) = "Raku!";
    is $str, "Hello Raku!", "substr-rw with from-only width reaches the slot";
}

{
    my $str = "abcdef";
    substr-rw($str, 2, 2) = "XY";
    substr-rw($str, 0, 1) = "Z";
    is $str, "ZbXYef", "two successive substr-rw assigns accumulate on the slot";
}

{
    # binding form: `my $r := substr-rw(...)` then assign through $r
    my $str = "gorch ding";
    my $r := substr-rw($str, 0, 5);
    $r = "gloop";
    is $str, "gloop ding", "bound substr-rw proxy assign reaches the slot";
}

{
    my $buf = Buf.new(1, 2, 3, 4, 5);
    subbuf-rw($buf, 1, 2) = Buf.new(9, 9);
    is $buf, Buf.new(1, 9, 9, 4, 5), "subbuf-rw lvalue assign reaches the local slot";
}

{
    my $str = "0123456789";
    substr-rw($str, 8, 2) = "ab";
    is $str, "01234567ab", "substr-rw at tail reaches the slot";
}
