use Test;

# ADR-17800's property test: strings with literal U+10FFFD sprinkled among
# text that looks like a utf8-c8 synthetic (`xFF`) and ordinary characters
# round-trip through `.ords`/`chrs` and every encoder, and keep one grapheme
# per literal marker; a source literal (raw or escaped) is the same string.

# No combining mark: a `.join` across one does not normalize yet (#17889).
my @alphabet = "\x[10FFFD]", "x", "F", "A", "0", "a", "é", "😀";
srand 17800;
my @strings = (^60).map: { (^(1..8).pick).map({ @alphabet.pick }).join };
@strings.push: "\x[10FFFD]xFF", "\x[10FFFD]\x[10FFFD]xFF", "a\x[10FFFD]", "\x[10FFFD]";

plan 6 * @strings + 4;

for @strings -> $s {
    my $ords = $s.ords.List;
    is-deeply $s.ords.chrs.ords.List, $ords, "ords/chrs round trip ({$ords})";
    is $s.encode('utf8').decode('utf8'), $s, "utf8 round trip ({$ords})";
    is $s.encode('utf16').decode('utf16'), $s, "utf16 round trip ({$ords})";
    is $s.encode('utf8-c8').decode('utf8-c8'), $s, "utf8-c8 round trip ({$ords})";
    is $s.NFD.Str.NFC.Str, $s.NFC.Str, "NFD/NFC through Uni ({$ords})";
    ok $ords.grep(0x10FFFD).elems <= $s.chars, "a literal marker is at least one grapheme ({$ords})";
}

is "􏿽xFF".chars, 4, 'a raw U+10FFFD in source text is a literal';
ok "􏿽xFF" eq "\x[10FFFD]xFF", 'raw and escaped spellings are the same string';
is "\x[10FFFD]".encode.list.elems, 4, 'one literal marker encodes as its four UTF-8 bytes';
ok Blob.new(255).decode('utf8-c8') ne "\x[10FFFD]xFF", 'the synthetic differs from the literal text';
