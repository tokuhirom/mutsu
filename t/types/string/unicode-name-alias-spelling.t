use Test;

plan 11;

# Multi-word Unicode name aliases match like Rakudo: case-insensitive and
# whitespace-trimmed, but hyphens, underscores and missing spaces do not match.
for ('BYTE ORDER MARK', 0xFEFF, 'LATIN CAPITAL LETTER GHA', 0x1A2, 'LINE FEED', 0xA) -> $name, $cp {
    is uniparse($name).ord, $cp, "$name resolves";
    is uniparse($name.lc).ord, $cp, "{$name.lc} resolves";
}
is uniparse(' BYTE ORDER MARK ').ord, 0xFEFF, 'surrounding whitespace is ignored';
for <byte-order-mark byteordermark BYTE_ORDER_MARK line-feed> -> $name {
    dies-ok { uniparse($name) }, "$name is not an alias spelling";
}
