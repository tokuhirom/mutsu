use Test;

plan 6;

# Buf.decode strips UTF-8 BOM
{
    my $buf = Buf.new(0xEF, 0xBB, 0xBF, 0x68, 0x69);
    my $decoded = $buf.decode('utf-8');
    is $decoded, 'hi', 'Buf.decode strips UTF-8 BOM';
    is $decoded.chars, 2, 'Buf.decode BOM-stripped string has correct char count';
}

# Buf.decode with only BOM
{
    my $buf = Buf.new(0xEF, 0xBB, 0xBF);
    my $decoded = $buf.decode('utf-8');
    is $decoded, '', 'Buf.decode of only BOM gives empty string';
    is $decoded.chars, 0, 'Buf.decode of only BOM has 0 chars';
}

# File slurp strips BOM
{
    my $file = "bom-test-local-$*PID";
    given open($file, :w) {
        .write(Buf.new(0xEF, 0xBB, 0xBF, 0x6F, 0x6B));
        .close;
    }
    my $content = slurp($file);
    is $content, 'ok', 'slurp strips UTF-8 BOM from file';
    is $content.chars, 2, 'slurp BOM-stripped string has correct char count';
    LEAVE unlink $file;
}
