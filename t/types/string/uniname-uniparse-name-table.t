use Test;

# `.uniname` / `uninames` and `\c[...]` / `uniparse` share the committed
# Unicode name table (#10438): stored names across planes, the algorithmic
# Hangul-syllable and CJK-ideograph families, UAX #44 LM2 loose matching, and
# the U+1180 / U+116C pair that LM2 singles out.

plan 18;

is 'a'.uniname, 'LATIN SMALL LETTER A', 'BMP name';
is 0x1F600.chr.uniname, 'GRINNING FACE', 'astral name';
is 0xAC00.chr.uniname, 'HANGUL SYLLABLE GA', 'first Hangul syllable';
is 0xD7A3.chr.uniname, 'HANGUL SYLLABLE HIH', 'last Hangul syllable';
is 0x4E00.chr.uniname, 'CJK UNIFIED IDEOGRAPH-4E00', 'CJK ideograph (BMP)';
is 0x20000.chr.uniname, 'CJK UNIFIED IDEOGRAPH-20000', 'CJK ideograph (plane 2)';
is 0x0.chr.uniname, '<control-0000>', 'control character';
is 0x378.chr.uniname, '<reserved-0378>', 'unassigned codepoint';
is 0x1180.chr.uniname, 'HANGUL JUNGSEONG O-E', 'name with a kept medial hyphen';
is uninames('a☃'), ('LATIN SMALL LETTER A', 'SNOWMAN'), 'uninames';

is "\c[LATIN SMALL LETTER A]", 'a', '\c[] exact name';
is "\c[latin small letter a]", 'a', '\c[] is case-insensitive';
is "\c[HANGUL SYLLABLE GAG]".ord.base(16), 'AC01', '\c[] Hangul syllable';
is "\c[CJK UNIFIED IDEOGRAPH-4E00]".ord.base(16), '4E00', '\c[] CJK ideograph';
is "\c[TIBETAN LETTER -A]".ord.base(16), 'F60', '\c[] name with a non-medial hyphen';
is uniparse('HANGUL JUNGSEONG O-E').ord.base(16), '1180', 'uniparse U+1180';
is uniparse('HANGUL JUNGSEONG OE').ord.base(16), '116C', 'uniparse U+116C';
is uniparse('BLACK STAR'), '★', 'uniparse';
