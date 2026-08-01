use Test;

# Base64 is bundled (BATTERIES.md §7, docs/batteries/base64.md), so it must
# load and work with a plain `use` -- no `-I`, no install. This pins the
# zero-config resolution and a smoke slice of the API; the exhaustive
# behaviour check is the release-time gate that runs the full upstream suite
# (scripts/battery-testsuite.sh).

plan 6;

use Base64;

is encode-base64("Hello, World!", :str), 'SGVsbG8sIFdvcmxkIQ==',
    'encode-base64 :str encodes a string';
is decode-base64('SGVsbG8sIFdvcmxkIQ==', :bin).decode, 'Hello, World!',
    'decode-base64 :bin round-trips';
is encode-base64(Blob.new(251, 255), :str), '+/8=', 'standard alphabet uses + and /';
is encode-base64(Blob.new(251, 255), :uri, :str), '-_8=', ':uri uses - and _';
is encode-base64("A", :str, :!pad), 'QQAA', ':!pad output matches upstream (raku-verified)';
is decode-base64(encode-base64(Buf.new(^256), :str), :bin).elems, 256,
    'all byte values round-trip';
