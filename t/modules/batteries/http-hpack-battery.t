use Test;

# HTTP::HPACK is bundled (BATTERIES.md §7, docs/batteries/http-hpack.md), so
# it must load and work with a plain `use` -- no `-I`, no install. This pins
# the zero-config resolution and a smoke slice of the API; the exhaustive
# behaviour check is the release-time gate that runs the full upstream suite
# (scripts/battery-testsuite.sh).

plan 5;

use HTTP::HPACK;

# RFC 7541 C.2.1: literal header field with incremental indexing.
my $packed = Buf.new(
    0x40, 0x0a, 0x63, 0x75, 0x73, 0x74, 0x6f, 0x6d, 0x2d, 0x6b, 0x65,
    0x79, 0x0d, 0x63, 0x75, 0x73, 0x74, 0x6f, 0x6d, 0x2d, 0x68, 0x65,
    0x61, 0x64, 0x65, 0x72);
my @headers = HTTP::HPACK::Decoder.new.decode-headers($packed);
is @headers.elems, 1, 'decoded one header';
is @headers[0].name, 'custom-key', 'header name decoded';
is @headers[0].value, 'custom-header', 'header value decoded';

my $encoded = HTTP::HPACK::Encoder.new.encode-headers([
    HTTP::HPACK::Header.new(name => 'custom-key', value => 'custom-header'),
]);
is-deeply $encoded.list, $packed.list, 'encoding round-trips the RFC example';

# An indexed static-table header (RFC 7541 C.2.4: :method GET is index 2).
my @static = HTTP::HPACK::Decoder.new.decode-headers(Buf.new(0x82));
is @static[0].name ~ '|' ~ @static[0].value, ':method|GET',
    'static-table indexed header decodes';
