use Test;

# An integer `uniprop` argument is a codepoint: negative ones die,
# ones past 0x10FFFF answer each property's default, and ones of 2**31
# or more (a BigInt included) answer "" for everything. The routine form
# is the method form (#13173).

plan 24;

for -1, -255, -(2**70) -> $cp {
    my $hex = $cp.abs.base(16);
    throws-like { $cp.uniprop }, X::AdHoc,
        message => "Codepoint $cp (0x-$hex) is out of bounds in 'uniprop'",
        "($cp).uniprop dies";
    throws-like { uniprop($cp) }, X::AdHoc,
        message => "Codepoint $cp (0x-$hex) is out of bounds in 'uniprop'",
        "uniprop($cp) dies";
}
throws-like { (-1).uniprop("Script") }, X::AdHoc,
    message => "Codepoint -1 (0x-1) is out of bounds in 'uniprop'",
    'uniprop with a property dies too';
throws-like { uniprop(-1, "Script") }, X::AdHoc,
    message => "Codepoint -1 (0x-1) is out of bounds in 'uniprop'",
    'routine uniprop with a property dies too';

# Past the last codepoint: per-property defaults.
is-deeply 0x110000.uniprop, "", '0x110000.uniprop is ""';
is-deeply uniprop(0x110000), "", 'uniprop(0x110000) is ""';
is-deeply 0x110000.uniprop("Block"), "", 'Block is ""';
is-deeply 0x110000.uniprop("Script"), "", 'Script is ""';
is-deeply 0x110000.uniprop("Alphabetic"), False, 'a binary property is False';
is-deeply 0x110000.uniprop("Numeric_Value"), Nil, 'Numeric_Value is Nil';
is-deeply 0x110000.uniprop("Name"), "<unassigned>", 'Name is <unassigned>';
is-deeply (2**31 - 1).uniprop("Alphabetic"), False, '2**31-1 still looks up';

# 2**31 and up: "" for every property, without wrapping.
is-deeply uniprop(2**32 + 65), "", '2**32+65 does not wrap to "A"';
is-deeply (2**31).uniprop("Alphabetic"), "", '2**31 answers ""';
is-deeply uniprop(2**70), "", 'a BigInt is a codepoint, not a string';
is-deeply (2**70).uniprop("Script"), "", 'BigInt with a property';

# The last codepoints and surrogates are unchanged.
is 0x10FFFF.uniprop, "Cn", '0x10FFFF is Cn';
is 0xD800.uniprop, "Cs", 'a surrogate is Cs';

# A Bool is the Int it stands for, in both forms.
is uniprop(True), "Cc", 'uniprop(True) reads codepoint 1';
is uniprop(True, "Script"), "Common", 'uniprop(True, "Script") reads codepoint 1';
