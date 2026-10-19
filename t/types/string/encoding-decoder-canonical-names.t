use Test;

for <utf8 UTF-8 utf16 utf-16 utf16le utf-16le utf16be utf-16be> -> $label {
    my $encoding = Encoding::Registry.find($label);
    is $encoding.name, $label.lc.subst('-', ''), "$label has the canonical UTF name";
    ok $encoding.alternative-names.grep(* eq $encoding.name).elems == 0,
        'alternative names exclude the canonical name';
    is 'AB'.encode($encoding.name).decode($label), 'AB',
        "$label canonical name round trips";
}
is Encoding::Registry.find('latin-1').name, 'iso-8859-1', 'Latin-1 canonical name';
is Encoding::Registry.find('ascii').name, 'ascii', 'ASCII canonical name';
is-deeply 'AB'.encode('utf-16be').list, (0, 65, 0, 66), 'big endian bytes';
is-deeply 'AB'.encode('utf-16le').list, (65, 0, 66, 0), 'little endian bytes';
throws-like { Blob.new(65).decode('not-an-encoding') }, X::AdHoc,
    message => / 'not-an-encoding' /, 'unknown encoding retains the requested name';
done-testing;
