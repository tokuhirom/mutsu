use Test;

plan 13;

# Unicode methods on a Nil receiver fail their signature (#13192).
for <uniprop uniprops unival univals uniname> -> $m {
    throws-like { Nil."$m"() }, X::Multi::NoMatch, "Nil.$m dies";
}
throws-like { Nil.unimatch("Lu") }, X::Multi::NoMatch, 'Nil.unimatch dies';
throws-like { uniname(Nil) }, X::Multi::NoMatch, 'uniname(Nil) dies';

# A property-less unimatch value names a block with underscores, not spaces.
ok "A".unimatch("Basic_Latin"), 'Basic_Latin names the block';
nok "A".unimatch("Basic Latin"), '"Basic Latin" does not';
nok 0x378.unimatch("Greek and Coptic"), 'nor "Greek and Coptic"';
ok 0xD800.unimatch("High_Surrogates"), 'High_Surrogates names the block of a surrogate';
nok 0xD800.unimatch("High Surrogates"), 'but "High Surrogates" does not';
# An unassigned codepoint is in No_Block.
ok 0x0378.unimatch("No_Block"), 'an unassigned codepoint is in No_Block';
