use Test;

plan 8;

# `.encode($encoding, $form)` applies a Unicode normalization form before
# encoding: C/NFC, D/NFD, KC/NFKC, KD/NFKD. Rakudo skips this in
# roast S32-str/encode.t ("We do not handle NDF yet"); mutsu implements it.
# (encode.t's own assertion compares with `Buf` via `eqv`, but `.encode`
# returns `utf8`, and `utf8 eqv Buf` is False even in Rakudo — so this checks
# the produced bytes directly.)

# 'ä' (U+00E4) decomposes under NFD to 'a' + U+0308 (combining diaeresis).
is-deeply 'ä'.encode('utf8', 'D').list, (0x61, 0xcc, 0x88),
    'encode with NFD decomposes the precomposed character';
is-deeply 'ä'.encode('utf8', 'NFD').list, (0x61, 0xcc, 0x88),
    'the full "NFD" name works too';

# NFC keeps / recomposes the precomposed form.
is-deeply 'ä'.encode('utf8', 'C').list, (0xc3, 0xa4),
    'encode with NFC keeps the precomposed character';
is-deeply "a\x[308]".encode('utf8', 'C').list, (0xc3, 0xa4),
    'NFC composes a base + combining mark';

# Default (no form) encodes as written.
is-deeply 'ä'.encode('utf8').list, (0xc3, 0xa4),
    'encode without a form leaves the string as-is';

# NFKD applies a compatibility decomposition (① -> "1").
is-deeply "\x[2460]".encode('utf8', 'KD').list, (0x31,),
    'encode with NFKD applies compatibility decomposition';

# An unrecognized second positional is ignored (bytes unchanged).
is-deeply 'abc'.encode('utf8', 'bogus').list, (0x61, 0x62, 0x63),
    'an unrecognized normalization form leaves the bytes unchanged';

# ASCII round-trips regardless of form.
is-deeply 'abc'.encode('utf8', 'D').list, (0x61, 0x62, 0x63),
    'ASCII is unchanged under NFD';
