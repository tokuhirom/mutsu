use Test;

# When .encode cannot represent a codepoint, Raku raises an X::AdHoc whose
# message names the encoding in its human-readable form and reports the
# offending codepoint in decimal:
#   "Error encoding ASCII string: could not encode codepoint 233"
# mutsu used a different wording ("character 'é' (U+00E9) is not representable")
# for ascii/latin1 and a hex codepoint for the other single-byte encodings.

plan 8;

sub encode-error($s, $enc) {
    my $msg;
    { $s.encode($enc); CATCH { default { $msg = .message } } }
    $msg;
}

is encode-error("café", "ascii"),
    "Error encoding ASCII string: could not encode codepoint 233",
    'ASCII error message (decimal codepoint)';
is encode-error("\x[2603]", "ascii"),
    "Error encoding ASCII string: could not encode codepoint 9731",
    'ASCII with astral-ish codepoint';
is encode-error("€", "latin1"),
    "Error encoding Latin-1 string: could not encode codepoint 8364",
    'Latin-1 via latin1 alias';
is encode-error("€", "iso-8859-1"),
    "Error encoding Latin-1 string: could not encode codepoint 8364",
    'Latin-1 via iso-8859-1';
is encode-error("\x[2603]", "windows-1252"),
    "Error encoding Windows-1252 string: could not encode codepoint 9731",
    'Windows-1252';
is encode-error("\x[2603]", "windows-1251"),
    "Error encoding Windows-1251 string: could not encode codepoint 9731",
    'Windows-1251';

# The raised exception is an X::AdHoc (matching raku)
my $type;
{ "café".encode("ascii"); CATCH { default { $type = .^name } } }
is $type, "X::AdHoc", 'encode error is X::AdHoc';

# Encodable strings still round-trip
is "hello".encode("ascii").decode("ascii"), "hello", 'ASCII round-trips';
