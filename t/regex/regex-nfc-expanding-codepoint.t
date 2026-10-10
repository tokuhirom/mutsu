use Test;

# From the P5quotemeta ecosystem distribution: U+2ADC is a composition
# exclusion, so the NFG subject holds it as the grapheme U+2ADD U+0338.
# A pattern naming the lone codepoint must match that grapheme.

plan 7;

my $s = 0x2adc.chr;
is $s.chars, 1, 'U+2ADC is one grapheme';
ok $s ~~ /\x[2adc]/, 'escape literal matches';
ok $s ~~ /"\x[2adc]"/, 'quoted literal matches';
ok $s ~~ /<[\x[2adc]]>/, 'char class singleton matches';
ok $s ~~ /<[\x[2adc] a]>/, 'char class with other items matches';
nok $s ~~ /<[\x[2794]..\x[2bff]]>/, 'a range does not match the grapheme';
is (S:g/(<[\x[2adc]]>)/\\$0/ given $s).chars, 2, 'substitution escapes it';

done-testing;
