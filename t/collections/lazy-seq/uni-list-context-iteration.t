use Test;

# A Uni (and its NFC/NFD/NFKC/NFKD forms) does `Positional[uint32]`: in list
# context it must flatten to its codepoints, not act as a single scalar item
# (Terminal::WCWidth's `$str.NFC.map(&wcwidth)` / `for $str.NFC { ... }`
# relies on this — mutsu previously handed the whole Uni value to the block
# as one item, so a `for`/`.map` callback typed `(Int:D $ucs)` died with
# "expected Int:D but got NFC").

plan 4;

my @codes;
for "ab".NFC { @codes.push($_) }
is @codes.join(","), "97,98", "for-loop over a Uni iterates its codepoints";
is @codes[0].^name, "Int", "each iterated element is a plain Int";

my @mapped = "ab".NFC.map({ $_ + 1 });
is @mapped.join(","), "98,99", ".map over a Uni iterates its codepoints";

sub only-int(Int:D $x) { $x }
is "ab".NFC.map(&only-int).join(","), "97,98",
    ".map binds each Uni codepoint to a typed Int:D parameter";
