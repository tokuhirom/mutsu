use Test;

for (
    ['utf8', "A\x[e9]\x[1f600]", 'utf8'],
    ['utf8-c8', "A\x[e9]", 'Blob[uint8]'],
    ['utf16', "A\x[1f600]", 'utf16'],
    ['utf16le', "A\x[1f600]", 'Blob[uint8]'],
    ['utf16be', "A\x[1f600]", 'Blob[uint8]'],
    ['ascii', 'AB', 'Blob[uint8]'],
    ['iso-8859-1', "A\x[e9]", 'Blob[uint8]'],
    ['windows-1251', "A\x[416]", 'Blob[uint8]'],
    ['windows-1252', "A\x[20ac]", 'Blob[uint8]'],
    ['windows-932', "A\x[3042]", 'Blob[uint8]'],
) -> @case {
    my ($encoding, $text, $type) = @case;
    my $got = Encoding::Registry.find($encoding).encoder.encode-chars($text);
    is $got.^name, $type, "$encoding buffer type";
    is-deeply $got.list, $text.encode($encoding).list,
        "$encoding shares Str.encode's codec";
    is $got.decode($encoding), $text, "$encoding round trip";
}

my $ascii = Encoding::Registry.find('ascii');
for <utf16 utf16le utf16be> -> $encoding {
    is-deeply Encoding::Registry.find($encoding).encoder(:replacement)
        .encode-chars("A\x[1f600]").list,
        "A\x[1f600]".encode($encoding).list,
        "$encoding keeps its codec when replacement is enabled";
}
dies-ok { $ascii.encoder.encode-chars("\x[e9]") }, 'ASCII encoding is strict by default';
dies-ok { $ascii.encoder(:!replacement).encode-chars("\x[e9]") },
    'False replacement keeps strict encoding';
is-deeply $ascii.encoder(:replacement).encode-chars("A\x[e9]").list,
    (65, 63), 'True replacement uses a question mark';
is-deeply $ascii.encoder(replacement => 'XY').encode-chars("A\x[e9]").list,
    (65, 88, 89), 'a replacement can contain multiple characters';
dies-ok { $ascii.encoder(replacement => "\x[e9]").encode-chars('A') },
    'an unencodable replacement is rejected';
for <iso-8859-1 windows-1252> -> $encoding {
    is-deeply Encoding::Registry.find($encoding).encoder(replacement => "\x[e9]")
        .encode-chars("A\x[100]").list,
        (65, 233), "$encoding encodes replacement text in its own codec";
}
done-testing;
