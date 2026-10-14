use Test;

# An integer argument of unival / unimatch / chr is a codepoint, bounds-checked
# the way rakudo does; univals always reads the string form. Every routine
# form is the method form, and `Nil` matches no routine candidate (#13174).

plan 40;

sub dies-oob(&code, $cp, $routine, $desc) {
    my $hex = ($cp < 0 ?? '-' !! '') ~ $cp.abs.base(16);
    throws-like &code, X::AdHoc,
        message => "Codepoint $cp (0x$hex) is out of bounds in '$routine'", $desc;
}

# unival
dies-oob { (-1).unival }, -1, 'unival', '(-1).unival dies';
dies-oob { unival(-1) }, -1, 'unival', 'unival(-1) dies';
dies-oob { (2**31).unival }, 2**31, 'unival', '2**31 is out of bounds';
dies-oob { (2**32 + 65).unival }, 2**32 + 65, 'unival', '2**32+65 does not wrap';
dies-oob { unival(2**70) }, 2**70, 'unival', 'a BigInt is a codepoint';
dies-oob { (-(2**70)).unival }, -(2**70), 'unival', 'a negative BigInt dies';
is-deeply 0x110000.unival, Nil, 'past 0x10FFFF is Nil';
is-deeply (2**31 - 1).unival, Nil, '2**31-1 is Nil';
ok 0xD800.unival.isNaN, 'a surrogate is NaN';
is unival(0x31), 1, 'unival(0x31)';
ok unival(True).isNaN, 'unival(True) reads codepoint 1';

# univals reads the string form, an Int's digits included
is-deeply 65.univals, (6, 5).Seq, '65.univals reads "65"';
is-deeply univals(65), (6, 5).Seq, 'univals(65) reads "65"';
is-deeply (-1).univals.map(*.raku), ('NaN', '1'), '(-1).univals reads "-1"';
is-deeply 0x110000.univals, (1, 1, 1, 4, 1, 1, 2).Seq, '0x110000.univals reads its digits';

# unimatch
dies-oob { unimatch(-1, "Lu") }, -1, 'unimatch', 'unimatch(-1, ...) dies';
dies-oob { (-1).unimatch("Lu", "gc") }, -1, 'unimatch', 'with a property too';
dies-oob { unimatch(-1, "Lu", "gc") }, -1, 'unimatch', 'routine with a property too';
ok 0x110000.unimatch("Cn"), 'past 0x10FFFF is Cn';
ok unimatch(0x110000, "C"), 'and in the major class C';
ok 0x110000.unimatch("Unknown", "sc"), 'with script Unknown';
ok 0x110000.unimatch("No_Block"), 'in No_Block';
ok 0x110000.unimatch("N", "East_Asian_Width"), 'with an unassigned codepoint\'s defaults';
ok 0x110000.unimatch("False", "Alphabetic"), 'binary properties are False';
nok 0x110000.unimatch("Alphabetic"), 'so the property itself does not match';
nok 0x110000.unimatch("Lu"), 'a real category does not match';
ok 0xD800.unimatch("Cs"), 'a surrogate is Cs';
nok 0xD800.unimatch("Cn"), 'not Cn';
nok (2**31).unimatch("Cn"), '2**31 matches nothing';
nok (2**70).unimatch("Cn"), 'nor does a BigInt';
ok unimatch(True, "Cc"), 'unimatch(True, ...) reads codepoint 1';
ok unimatch(True, "Cc", "gc"), 'with a property too';

# chr
dies-oob { chr(-1) }, -1, 'chr', 'chr(-1) reports 0x-1';
dies-oob { chr(-255) }, -255, 'chr', 'chr(-255) reports 0x-FF';
throws-like { chr(0x110000) }, X::AdHoc,
    message => 'chr codepoint 1114112 (0x110000) is out of bounds',
    'past 0x10FFFF is MoarVM\'s message';
dies-oob { chr(2**31) }, 2**31, 'chr', '2**31 is rakudo\'s message';
is chr("65"), 'A', 'a Str argument numifies';

# Nil matches no routine candidate
for &unival, &univals, &chr -> &r {
    throws-like { r(Nil) }, Exception, message => /'Cannot resolve caller'/,
        "{&r.name}(Nil) dies";
}
