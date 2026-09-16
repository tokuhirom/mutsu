use v6;
use Test;

# `.map`/`.grep`/`.first` on a Uni/NFC/NFD/NFKC/NFKD value iterate its OWN
# codepoints as plain Int values, regardless of how many codepoints it holds
# -- including the single-codepoint case, where the whole-value fallback
# used to look like a correct one-element list and hid the bug (#8517).
# A `for $n -> $c` loop over the SAME value (a $ scalar, not flattened)
# correctly stays a single item (Rakudo's single-argument rule) and must
# not regress.

plan 8;

{
    sub ascii-escape(Int $cp --> Str) { sprintf '\\u%04x', $cp }
    is '日'.NFC.map(&ascii-escape).join, '\\u65e5',
        '.map binds a single-codepoint NFC element to a typed Int parameter';
}

{
    my $n = 'ab'.NFC;
    is $n.map({ $_ }).join(','), '97,98', '.map decomposes a multi-codepoint NFC';
    is $n.grep({ $_ > 0 }).join(','), '97,98', '.grep decomposes a multi-codepoint NFC';
    is $n.first(*.defined), 97, '.first decomposes a multi-codepoint NFC';
}

{
    my $n = '日'.NFC;
    is $n.grep({ True }).join(','), '26085', '.grep decomposes a single-codepoint NFC';
    is $n.first(*.defined), 26085, '.first decomposes a single-codepoint NFC';
}

{
    # The single-argument rule: a bare `for` over a $-sigiled scalar treats
    # the whole Uni as one item, unlike an explicit method call.
    my $n = '日'.NFC;
    my $count = 0;
    for $n -> $c { $count++; is $c.^name, 'NFC', 'for over a scalar Uni yields the whole value, not its codepoints'; }
    is $count, 1, 'for over a scalar Uni runs its body exactly once';
}
